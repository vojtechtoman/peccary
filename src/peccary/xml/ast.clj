(ns peccary.xml.ast
  (:gen-class)
  (:require
            [clojure.zip :as zip]
            [peccary.xml :refer :all]
            [peccary.util :refer :all]
            [name.choi.joshua.fnparse :as fp]))


;;; 

(defn flatten-and-filter 
  [x]
  (remove nil? (flatten x)))

(defn- namespace-bindings
  [evt]
  (let [attrs (:attrs evt)
        ns-attrs (filter (fn [[qname _]]
                           (ns-decl? qname))
                         attrs)]
    (reduce (fn [decls [qname val]]
              (assoc decls (ns-prefix qname) val))
            {}
            ns-attrs)))

(defn- base-uri
  [evt]
  (when-let [xml-base (-> evt :attrs (get qn-xml-base))]
    xml-base))

(defn nested-context
  [ctx evt]
  (let [evt-ns-decls (namespace-bindings evt)
        ctx-base-uri (:base-uri ctx)
        evt-base-uri (base-uri evt)
        base-uri (resolve-uri ctx-base-uri evt-base-uri)]
    (-> ctx
        (update-in [:ns-context] (partial merge evt-ns-decls))
        (assoc :base-uri base-uri))))

(defn- ign-ws-rf 
  [ctx]
  (fp/term (fn [evt]
             (and (= :text (:type evt))
                  (when-let [data (:data evt)]
                    (empty? (clojure.string/trim data)))))))

(defn- ign-comm-rf 
  [ctx]
  (fp/term (fn [evt]
             (= :comment (:type evt)))))

(defn- ign-pi-rf 
  [ctx]
  (fp/term (fn [evt]
             (= :pi (:type evt)))))

(defn ignorable-rf
  [ctx]
  (fp/constant-semantics (fp/rep* (fp/alt (ign-ws-rf ctx)
                                          (ign-comm-rf ctx)
                                          (ign-pi-rf ctx))) :ignorable))

;;; 

(defn- doc-start-rf 
  [ctx]                                 ;ctx not needed
  (fp/term (fn [evt]
             (= :start-document (:type evt)))))

(defn- doc-end-rf 
  [ctx]                                 ;ctx not needed
  (fp/term (fn [evt]
             (= :end-document (:type evt)))))

(defn- non-elt-rf 
  [ctx]                                 ;ctx not needed
  (fp/term (fn [evt]
             (and (not= :start-element (:type evt))
                  (not= :end-element (:type evt))))))

(defn elt-start-rf 
  [ctx]                                 ;ctx not needed
  (fp/term (fn [evt]
             (= :start-element (:type evt)))))

(defn elt-end-rf 
  [ctx qname]                           ;ctx not needed
  (fp/term (fn [evt]
             (let [ename (:qname evt)
                   etype (:type evt)]
               (and
                (= :end-element etype)
                (= ename qname))))))

(defn elt-rf 
  [ctx]
  (fp/complex [selt (elt-start-rf ctx)
               content (let [nctx (nested-context ctx selt)]
                         (fp/rep* (fp/alt (elt-rf ctx)
                                          (non-elt-rf nctx))))
               eelt (elt-end-rf ctx (:qname selt))]
              [selt content eelt]))

;;; creates a "document" rule based on provided rule
(defn- doc-wrapper-rf
  [model-rf ctx]
  (fp/complex [sdoc (doc-start-rf ctx)
               c (let [nctx (nested-context ctx sdoc)]
                   (model-rf nctx))
               edoc (doc-end-rf ctx)]
              c))

;;; creates a rule that accepts model-rf either directly or wrappe din a document
(defn opt-doc-rf
  [model-rf ctx]
  (fp/alt (model-rf ctx)
          (doc-wrapper-rf model-rf ctx)))

(defn well-formed-content-rf 
  [ctx]
  (fp/conc (fp/rep* (non-elt-rf ctx))
           (elt-rf ctx)
           (fp/rep* (non-elt-rf ctx))))

(defn well-formed-doc-rf 
  [ctx]
  (doc-wrapper-rf well-formed-content-rf ctx))

(defmacro defelt
  [var constructor & [model-rf elt-validator]]
  ;; var - the var to bind
  ;; constructor - a fn[selt content], produces an ast instance
  ;; model-rf a fn[ctx], produces a fnparse rule
  ;; validator - a fn[selt], a boolean function that validates selt

  ;; TODO avoid repeated construction of the nested context somehow
  (cond
   (not (symbol? var)) (throw (IllegalArgumentException. "var must be a symbol"))
   :else (let [ctx# (gensym "ctx")
               selt# (gensym "selt")
               eelt# (gensym "eelt")
               content# (gensym "content")
               ign# ['_ `(fp/opt (ignorable-rf ~ctx#))]
               model# (if (nil? model-rf) `fp/emptiness `(~model-rf (nested-context ~ctx# ~selt#)))
               elt-start-rf# (if (nil? elt-validator)
                              `(elt-start-rf ~ctx#)
                              `(fp/validate (elt-start-rf ~ctx#) ~elt-validator))
               elt-end-rf# `(elt-end-rf ~ctx# (:qname ~selt#))]
           `(defn ~var 
              [~ctx#]
              (fp/complex [~@(concat ign#
                                     [selt# `~elt-start-rf#]
                                     [content# `~model#]
                                     [eelt# `~elt-end-rf#]
                                     ign#)]
                          (let [content-flattened# (flatten-and-filter (list ~content#))
                                selt-context# (nested-context ~ctx# ~selt#)]
                            (~constructor selt-context# ~selt# content-flattened#)))))))

(defn- location-str
  [evt]
  (let [loc (:location evt)
        res (or (:resource loc) "<unknown>")
        column (or (:column loc) "?")
        line (or (:line loc) "?")
        offset (or (:offset loc) "?")]
    (str res " (column:" column " line:" line " offset:" offset ")")))

(defn- parse-failure
  [state]
  (let [remainder (:remainder state)
        next (first remainder)
        loc (location-str next)]
    (printf "Parse failure at %s: %s\n" loc next)))

(defn- parse-incomplete
  [orig-state new-state]
  (let [remainder (:remainder new-state)
        next (first remainder)
        loc (location-str next)]
    (printf "Leftover content at %s: %s\n" loc remainder)))

(defn- make-parser 
  [rf]
  (fn parser [evts & [ctx]]
    (let [initial-ctx (or ctx {})
          rule (rf initial-ctx)]
      (fp/rule-match rule
                     parse-failure
                     parse-incomplete
                     {:remainder evts}))))

(defn parse
  [rf evts & [ctx]]
  (let [parser (make-parser rf)]
    (parser evts ctx)))


;;; 

;;; miscellaneous helper methods

(defn content
  [node]
  (:content node))

(defn cempty?
  [node]
  (empty? (content node)))

(defn cassoc
  [node c]
  (let [coll (if (sequential? c) c (list c))]
    (assoc node :content coll)))

(defn cset
  [node coll]
  (cassoc node coll))

(defn cprepend
  [node coll]
  (let [content (content node)
        new-content (concat coll content)]
    (cset node new-content)))

(defn cmap
  [pred? f node]
  (let [content (content node)
        new-content (map (fn [n]
                           (if (pred? n)
                             (f n)
                             n))
                         content)]
    (cassoc node new-content)))

(defn cgroup-by
  [f node]
  (let [content (content node)]
    (group-by f content)))

(defn cfilter
  [pred? node]
  (let [content (content node)]
    (filter pred? content)))

(defn cremove
  [pred? node]
  (let [content (content node)]
    (remove pred? content)))


(defn- make-ast-zipper
  [ast]
  (zip/zipper (fn [n]
                (not (cempty? n)))
              content
              (fn [n c]
                (cassoc n c))
              ast))

(defn- apply-editors
  [loc state editors]
  (let [node (zip/node loc)
        ectx (reduce (fn [ctx editor]
                       (let [state (:state ctx)
                             node (:node ctx)]
                         (merge {:state state
                                 :node node}
                                (editor state node))))
                     {:state state
                      :node node}
                     editors)
        estate (:state ectx)
        enode (:node ectx)
        eloc (zip/replace loc enode)]
    {:loc eloc
     :state estate}))

(defn ast-edit
  [ast & [initial-state pre-editors post-editors]]
  (let [z (make-ast-zipper ast)]
    (loop [loc z
           stack (list initial-state)
           dir :down]
      (if (= :down dir)
        ;; :down -> apply pre-editors and descend
        (let [state (first stack)
              ectx (apply-editors loc state pre-editors)
              eloc (:loc ectx)
              estate (:state ectx)
              down (zip/down eloc)]
          ;; recur with the first child if available, otherwise with the next sibling
          (if (nil? down)
            (recur eloc stack :next)
            (recur down (cons estate stack) :down)))
        ;; :next -> apply the post editors, then proceed with the next sibling
        (let [state (first stack)
              ectx (apply-editors loc state post-editors)
              eloc (:loc ectx)
              estate (:state ectx)
              next (zip/right eloc)]
          ;; recur with next sibling if available, otherwise go up
          (if (nil? next)
            (if-let [parent (zip/up eloc)]
              (recur parent (rest stack) :next) ;go up
              (zip/node eloc))           ;no parent -> we are done
            (recur next (cons estate (rest stack)) :down)))))))

