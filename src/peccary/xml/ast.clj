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

(defn- ign-ws-g 
  [ctx]
  (fp/term (fn [evt]
             (and (= :text (:type evt))
                  (when-let [data (:data evt)]
                    (empty? (clojure.string/trim data)))))))

(defn- ign-comm-g 
  [ctx]
  (fp/term (fn [evt]
             (= :comment (:type evt)))))

(defn- ign-pi-g 
  [ctx]
  (fp/term (fn [evt]
             (= :pi (:type evt)))))

(defn ignorable-g
  [ctx]
  (fp/constant-semantics (fp/rep* (fp/alt (ign-ws-g ctx)
                                          (ign-comm-g ctx)
                                          (ign-pi-g ctx))) :ignorable))

;;; 

(defn- doc-start-g 
  [ctx]                                 ;ctx not needed
  (fp/term (fn [evt]
             (= :start-document (:type evt)))))

(defn- doc-end-g 
  [ctx]                                 ;ctx not needed
  (fp/term (fn [evt]
             (= :end-document (:type evt)))))

(defn- non-elt-g 
  [ctx]                                 ;ctx not needed
  (fp/term (fn [evt]
             (and (not= :start-element (:type evt))
                  (not= :end-element (:type evt))))))

(defn elt-start-g 
  [ctx]                                 ;ctx not needed
  (fp/term (fn [evt]
             (= :start-element (:type evt)))))

(defn elt-end-g 
  [ctx qname]                           ;ctx not needed
  (fp/term (fn [evt]
             (let [ename (:qname evt)
                   etype (:type evt)]
               (and
                (= :end-element etype)
                (xmlutil/qn-eq ename qname))))))

(defn elt-g 
  [ctx]
  (fp/complex [selt (elt-start-g ctx)
               content (let [nctx (nested-context ctx selt)]
                         (fp/rep* (fp/alt (elt-g ctx)
                                          (non-elt-g nctx))))
               eelt (elt-end-g ctx (:qname selt))]
              [selt content eelt]))

;;; creates a "document" rule based on provided rule
(defn- doc-wrapper-g
  [model-g ctx]
  (fp/complex [sdoc (doc-start-g ctx)
               c (let [nctx (nested-context ctx sdoc)]
                   (model-g nctx))
               edoc (doc-end-g ctx)]
              c))

;;; creates a rule that accepts model-g either directly or wrappe din a document
(defn opt-doc-g
  [model-g ctx]
  (fp/alt (model-g ctx)
          (doc-wrapper-g model-g ctx)))

(defn well-formed-content-g 
  [ctx]
  (fp/conc (fp/rep* (non-elt-g ctx))
           (elt-g ctx)
           (fp/rep* (non-elt-g ctx))))

(defn well-formed-doc-g 
  [ctx]
  (doc-wrapper-g well-formed-content-g ctx))

(defmacro defelt
  [var constructor & [model-g elt-validator]]
  ;; var - the var to bind
  ;; constructor - a fn[selt content], produces an ast instance
  ;; model-g a fn[ctx], produces a fnparse rule
  ;; validator - a fn[selt], a boolean function that validates selt
  (cond
   (not (symbol? var)) (throw (IllegalArgumentException. "var must be a symbol"))
   :else (let [ctx# (gensym "ctx")
               selt# (gensym "selt")
               eelt# (gensym "eelt")
               content# (gensym "content")
               ign# ['_ `(fp/opt (ignorable-g ~ctx#))]
               model# (if (nil? model-g) `fp/emptiness `(~model-g (nested-context ~ctx# ~selt#)))
               elt-start-g# (if (nil? elt-validator)
                              `(elt-start-g ~ctx#)
                              `(fp/validate (elt-start-g ~ctx#) ~elt-validator))
               elt-end-g# `(elt-end-g ~ctx# (:qname ~selt#))]
           `(defn ~var 
              [~ctx#]
              (fp/complex [~@(concat ign#
                                     [selt# `~elt-start-g#]
                                     [content# `~model#]
                                     [eelt# `~elt-end-g#]
                                     ign#)]
                          (let [content-flattened# (flatten-and-filter (list ~content#))]
                            (~constructor ~selt# content-flattened#)))))))

(defn- parse-failure [state]
  (let [remainder (:remainder state)
        next (first remainder)
        uri (or (:uri next) "<unknown>")
        column (or (:column next) "?")
        line (or (:line next) "?")]
    (printf "Parse failure at %s (%s:%s): %s\n" uri column line next)))

(defn- parse-incomplete [orig-state new-state]
  (let [remainder (:remainder new-state)
        next (first remainder)
        uri (or (:uri next) "<unknown>")
        column (or (:column next) "?")
        line (or (:line next) "?")]
    (printf "Leftover content at %s (%s:%s): %s\n" uri column line remainder)))

(defn- make-parser 
  [parser-g]
  (fn paserfn [evts & [ctx]]
    (let [initial-ctx (or ctx {})
          parser (parser-g initial-ctx)]
      (fp/rule-match parser
                     parse-failure
                     parse-incomplete
                     {:remainder evts}))))

(defn parse
  [parser-g evts & [ctx]]
  (let [parser (make-parser parser-g)]
    (parser evts ctx)))


