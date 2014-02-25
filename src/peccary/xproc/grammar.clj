(ns peccary.xproc.grammar
  (:gen-class)
  (:require [peccary.xml :refer :all]
            [peccary.xml.ast :as xmlast]
            [peccary.xproc.error :refer :all]
            [peccary.xproc.vocabulary :refer :all]
            [name.choi.joshua.fnparse :as fp]))


;;; The XProc grammar
;;; TODO (p:)use-when, p:documentation, and p:pipeinfo

(defn- extension-attr-name?
  [qname]
  (when-let [ns-uri (ns-uri qname)]
    (not (ns-decl? qname))))

(defn- create-xproc-elt-validator
  [qname & [attrspec]]
  (fn xproc-elt-v [evt]
    (let [ename (:qname evt)
          eattrs (:attrs evt)
          required (into {} (filter (fn [[_ spec]]
                                      (= spec :required))
                                    attrspec))
          unknown (reduce #(dissoc %1 %2)
                          eattrs
                          (keys attrspec))]
      (and
       ;; the name matches
       (= qname ename)
       ;; all of the unknown attributes are extension attributes
       (or (empty? (remove (fn [[attr-name _]]
                             (or (extension-attr-name? attr-name)
                                 (ns-decl? attr-name)))
                           unknown))
           (err-XS0008))
       ;; all required specified
       (or (empty? (reduce #(dissoc %1 %2)
                           required
                           (keys eattrs)))
           (err-XS0038))))))

(defn- attr-name-type
  [qname]
  (cond
   (extension-attr-name? qname) :extension
   (ns-decl? qname) :namespace-declaration
   :else :regular))

(defn- create-xproc-ast-constructor
  [type]
  (fn xproc-ast-c [ctx selt content]
    (let [attrs-grouped (group-by (fn [[attr-name _]]
                                    (attr-name-type attr-name))
                                  (:attrs selt))
          regular-attrs (into {} (:regular attrs-grouped))
          extension-attrs (into {} (:extension attrs-grouped))
          loc (:location selt)
          posname (:posname selt)]
      {:type type
       :content content
       :attrs regular-attrs
       :extension-attrs extension-attrs
       :ctx (assoc ctx :location loc :posname posname)})))

(defmacro defxprocelt
  [var {qname :qname attrs :attrs model-rf :model type :type}]
  {:pre [(not (nil? qname))]}
  (let [ctype (or type `(keyword (local-name ~qname)))
        constructor `(create-xproc-ast-constructor ~ctype)
        elt-validator `(create-xproc-elt-validator ~qname ~attrs)]
    `(xmlast/defelt ~var ~constructor ~model-rf ~elt-validator)))

;;; 

(defxprocelt documentation-rf {:qname qn-e-documentation 
                               :model #(fp/rep* (xmlast/well-formed-content-rf %))})

(defxprocelt pipeinfo-rf {:qname qn-e-pipeinfo
                          :model #(fp/rep* (xmlast/well-formed-content-rf %))})

(defxprocelt empty-rf {:qname qn-e-empty})

(defxprocelt pipe-rf {:qname qn-e-pipe
                      :attrs {qn-a-step :required
                              qn-a-port :required}})

(defxprocelt document-rf {:qname qn-e-document
                          :attrs {qn-a-href :required}})

(defxprocelt data-rf {:qname qn-e-data
                      :attrs {qn-a-href :required
                              qn-a-wrapper :optional
                              qn-a-wrapper-prefix :optional
                              qn-a-wrapper-namespace :optional
                              qn-a-content-type :optional}})

(defxprocelt inline-rf {:qname qn-e-inline
                        :attrs {qn-a-exclude-inline-prefixes :optional}
                        :model #(xmlast/well-formed-content-rf %)})

(defn- connection-rf 
  [ctx]
  (fp/alt (empty-rf ctx)
          (document-rf ctx)
          (inline-rf ctx)
          (data-rf ctx)
          (pipe-rf ctx)))

(defxprocelt namespaces-rf {:qname qn-e-namespaces
                            :attrs {qn-a-binding :optional
                                    qn-a-element :optional
                                    qn-a-except-prefixes :optional}})

(defxprocelt variable-rf {:qname qn-e-variable
                          :attrs {qn-a-name :required
                                  qn-a-select :required}
                          :model #(fp/conc (fp/opt (connection-rf %))
                                           (fp/rep* (namespaces-rf %)))})

(defxprocelt with-option-rf {:qname qn-e-with-option
                             :attrs {qn-a-name :required
                                     qn-a-select :required}
                             :model #(fp/conc (fp/opt (connection-rf %))
                                              (fp/rep* (namespaces-rf %)))})

(defxprocelt with-param-rf {:qname qn-e-with-param
                            :attrs {qn-a-name :required
                                    qn-a-select :required
                                    qn-a-port :optional}
                            :model #(fp/conc (fp/opt (connection-rf %))
                                             (fp/rep* (namespaces-rf %)))})

(defxprocelt option-rf {:qname qn-e-option
                        :attrs {qn-a-name :required
                                qn-a-required :optional
                                qn-a-select :optional}})

(defxprocelt log-rf {:qname qn-e-log
                     :attrs {qn-a-port :required
                             qn-a-href :optional}})

(defxprocelt input-rf {:qname qn-e-input
                       :attrs {qn-a-port :required
                               qn-a-select :optional}
                       :model #(fp/opt (fp/alt (empty-rf %)
                                               (fp/rep+ (fp/alt (pipe-rf %)
                                                                (document-rf %)
                                                                (inline-rf %)
                                                                (data-rf %)))))})

;; TODO atomic steps do not allow connections! (XS0042)- perhaps check while traversing the AST?
(defxprocelt input-decl-rf {:qname qn-e-input
                            :attrs {qn-a-port :required
                                    qn-a-sequence :optional
                                    qn-a-primary :optional
                                    qn-a-kind :optional
                                    qn-a-select :optional}
                            :model #(fp/opt (fp/alt (empty-rf %)
                                                    (fp/alt (document-rf %)
                                                            (inline-rf %)
                                                            (data-rf %))))})

;; TODO atomic steps do not allow connections! (XS0029)- perhaps check while traversing the AST?
(defxprocelt output-decl-rf {:qname qn-e-output
                             :attrs {qn-a-port :required
                                     qn-a-sequence :optional
                                     qn-a-primary :optional}
                             :model #(fp/opt (fp/alt (empty-rf %)
                                                     (fp/alt (pipe-rf %)
                                                             (document-rf %)
                                                             (inline-rf %)
                                                             (data-rf %))))})

(defxprocelt iteration-source-rf {:qname qn-e-iteration-source
                                  :attrs {qn-a-select :optional}
                                  :model #(fp/opt (fp/alt (empty-rf %)
                                                          (fp/alt (pipe-rf %)
                                                                  (document-rf %)
                                                                  (inline-rf %)
                                                                  (data-rf %))))})

(declare subpipeline-rf)                 ;forward declaration

(defxprocelt for-each-rf {:qname qn-e-for-each
                          :attrs {qn-a-name :optional}
                          :model #(fp/conc (fp/opt (iteration-source-rf %))
                                           (fp/rep* (fp/alt (output-decl-rf %)
                                                            (log-rf %)))
                                           (subpipeline-rf %))})

(defxprocelt viewport-source-rf {:qname qn-e-viewport-source
                                 :model #(fp/opt (fp/alt (pipe-rf %)
                                                         (document-rf %)
                                                         (inline-rf %)
                                                         (data-rf %)))})

(defxprocelt viewport-rf {:qname qn-e-viewport
                          :attrs {qn-a-name :optional
                                  qn-a-match :required}
                          :model #(fp/conc (fp/opt (viewport-source-rf %))
                                           (fp/opt (output-decl-rf %))
                                           (fp/opt (log-rf %))
                                           (subpipeline-rf %))})

(defxprocelt xpath-context-rf {:qname qn-e-xpath-context
                               :model #(fp/alt (empty-rf %)
                                               (pipe-rf %)
                                               (document-rf %)
                                               (inline-rf %)
                                               (data-rf %))})

(defxprocelt when-rf {:qname qn-e-when
                      :attrs {qn-a-test :required}
                      :model #(fp/conc (fp/opt (xpath-context-rf %))
                                       (fp/rep* (fp/alt (output-decl-rf %)
                                                        (log-rf %)))
                                       (subpipeline-rf %))})

(defxprocelt otherwise-rf {:qname qn-e-otherwise
                           :model #(fp/conc (fp/rep* (fp/alt (output-decl-rf %)
                                                             (log-rf %)))
                                            (subpipeline-rf %))})

(defxprocelt choose-rf {:qname qn-e-choose
                        :attrs {qn-a-name :optional}
                        :model #(fp/conc (fp/opt (xpath-context-rf %))
                                         (fp/rep* (variable-rf %))
                                         (fp/rep* (when-rf %))
                                         (fp/opt (otherwise-rf %))
                                         (subpipeline-rf %))})

(defxprocelt group-rf {:qname qn-e-group
                       :attrs {qn-a-name :optional}
                       :model #(fp/conc (fp/rep* (fp/alt (output-decl-rf %)
                                                         (log-rf %)))
                                        (subpipeline-rf %))})

(defxprocelt catch-rf {:qname qn-e-catch
                       :attrs {qn-a-name :optional}
                       :model #(fp/conc (fp/rep* (fp/alt (output-decl-rf %)
                                                         (log-rf %)))
                                        (subpipeline-rf %))})

(defxprocelt try-rf {:qname qn-e-try
                     :attrs {qn-a-name :optional}
                     :model #(fp/conc (fp/rep* (variable-rf %))
                                      (group-rf %)
                                      (catch-rf %))})

(xmlast/defelt step-rf
  (fn xproc-step-ast-c [ctx selt content]
    (let [qname (:qname selt)
          c (create-xproc-ast-constructor :step)
          m (c ctx selt content)]
      (-> m (assoc :step-type qname))))
  #(fp/rep* (fp/alt (input-rf %)        ;TODO support ignorable inside step?
                    (with-option-rf %)
                    (with-param-rf %)
                    (log-rf %)))
  (fn step-elt-v [evt]
    (not (nil? (ns-uri (:qname evt))))))

(defn- subpipeline-rf 
  [ctx]
  (fp/conc (fp/rep* (variable-rf ctx))
           (fp/rep+ (fp/alt (for-each-rf ctx)
                            (viewport-rf ctx)
                            (choose-rf ctx)
                            (group-rf ctx)
                            (try-rf ctx)
                            (step-rf ctx)))))

(defxprocelt import-rf {:qname qn-e-import
                        :attrs {qn-a-href :required}})

(defxprocelt serialization-rf {:qname qn-e-serialization
                               :attrs {qn-a-port :required
                                       qn-a-byte-order-mark :optional
                                       qn-a-cdata-section-elements :optional
                                       qn-a-doctype-public :optional
                                       qn-a-doctype-system :optional
                                       qn-a-encoding :optional
                                       qn-a-escape-uri-attributes :optional
                                       qn-a-include-content-type :optional
                                       qn-a-indent :optional
                                       qn-a-media-type :optional
                                       qn-a-method :optional
                                       qn-a-normalization-form :optional
                                       qn-a-omit-xml-declaration :optional
                                       qn-a-standalone :optional
                                       qn-a-undeclare-prefixes :optional
                                       qn-a-version :optional}})

(declare declare-step-rf)                ;forward declaration
(defxprocelt pipeline-rf {:qname qn-e-pipeline
                          :attrs {qn-a-name :optional
                                  qn-a-type :optional
                                  qn-a-psvi-required :optional
                                  qn-a-xpath-version :optional
                                  qn-a-exclude-inline-prefixes :optional
                                  qn-a-version :optional} ;required for top-level pipelines (XS0062)
                          :model #(fp/conc (fp/rep* (fp/alt (input-decl-rf %)
                                                            (output-decl-rf %)
                                                            (option-rf %)
                                                            (log-rf %)
                                                            (serialization-rf %)))
                                           (fp/rep* (fp/alt (declare-step-rf %)
                                                            (pipeline-rf %)
                                                            (import-rf %)))
                                           (subpipeline-rf %))})

(defxprocelt declare-step-rf {:qname qn-e-declare-step
                              :attrs {qn-a-name :optional
                                      qn-a-type :optional
                                      qn-a-psvi-required :optional
                                      qn-a-xpath-version :optional
                                      qn-a-exclude-inline-prefixes :optional
                                      qn-a-version :optional} ;required for top-level pipelines (XS0062)
                              :model #(fp/conc (fp/rep* (fp/alt (input-decl-rf %)
                                                                (output-decl-rf %)
                                                                (option-rf %)
                                                                (log-rf %)
                                                                (serialization-rf %)))
                                               (fp/opt (fp/conc (fp/rep* (fp/alt (declare-step-rf %)
                                                                                 (pipeline-rf %)
                                                                                 (import-rf %)))
                                                                (subpipeline-rf %))))})

(defxprocelt library-rf {:qname qn-e-library
                         :attrs {qn-a-psvi-required :optional
                                 qn-a-xpath-version :optional
                                 qn-a-exclude-inline-prefixes :optional
                                 qn-a-version :optional}  ;required for top-level libraries (XS0062)
                         :model #(fp/rep* (fp/alt (import-rf %)
                                                  (declare-step-rf %)
                                                  (pipeline-rf %)))})

;;; 

(defn import-target-rf 
  [ctx]
  (xmlast/opt-doc-rf #(fp/alt (pipeline-rf %)
                              (declare-step-rf %)
                              (library-rf %))
                     ctx))

(defn main-pipeline-rf 
  [ctx]
  (xmlast/opt-doc-rf #(fp/alt (pipeline-rf %)
                              (declare-step-rf %))
                     ctx))

(defn import-target-rf 
  [ctx]
  (xmlast/opt-doc-rf #(fp/alt (pipeline-rf %)
                              (declare-step-rf %)
                              (library-rf %))
                     ctx))

(defn- xproc-pos
  [s]
  (->> s
       reverse
       (clojure.string/join ".")
       (str "!")))

(defn wrap
  [evts & [s]]
  (lazy-seq
   (if-let [evt (first evts)]
     (let [type (:type evt)]
       (cond
        (= :start-element type)
        (let [head (first s)
              ps (if (nil? head)
                   '(1)
                   (cons (inc head) (rest s)))
              xpos (xproc-pos ps)
              ns (cons 0 ps)
              decorated (assoc evt :posname xpos)]
          (cons decorated (wrap (rest evts) ns)))
        (= :end-element type)
        (cons evt (wrap (rest evts) (rest s)))
        
        :else
        (cons evt (wrap (rest evts) s))))
     ())))

(defn parse
  [rf evts & [ctx]]
  (xmlast/parse rf (wrap evts) ctx))
