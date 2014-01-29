(ns peccary.xml.ast
  (:gen-class)
  (:require [peccary.xml.util :as xmlutil]
            [name.choi.joshua.fnparse :as fp]))


;;; 

(defn flatten-and-filter 
  [x]
  (into [] (filter #(not (nil? %)) (flatten x))))

(defn- namespace-bindings
  [evt]
  (let [attrs (:attrs evt)
        ns-attrs (filter (fn [[qname _]]
                           (xmlutil/namespace-declaration? qname)) attrs)]
    (reduce (fn [decls [qname val]]
              (assoc decls (xmlutil/namespace-prefix qname) val)) {} ns-attrs)))

(defn- base-uri
  [evt]
  (when-let [xml-base (-> evt :attrs (get xmlutil/qn-xml-base))]
    xml-base))

(defn nested-context
  [ctx evt]
  (let [evt-ns-decls (namespace-bindings evt)
        ctx-base-uri (:base-uri ctx)
        evt-base-uri (base-uri evt)
        base-uri (xmlutil/resolve-uri ctx-base-uri evt-base-uri)]
    (-> ctx
        (update-in [:ns-context] #(merge % evt-ns-decls))
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
                          (let [content-flattened# (flatten-and-filter (list ~content#))]
                            (~constructor ~selt# content-flattened#)))))))

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

