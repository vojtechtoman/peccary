(ns peccary.xproc.ast
  (:gen-class)
  (:require [peccary.xml.util :as xmlutil]
            [peccary.xml.ast :as xmlast]
            [peccary.xproc.vocabulary :refer :all]
            [name.choi.joshua.fnparse :as fp]))


;;; TODO (p:)use-when, p:documentation, and p:pipeinfo


(defn- extension-attr-name? 
  [qname]
  (not (nil? (xmlutil/ns-uri qname))))

(defn- xproc-elt-validator-f 
  [qname & [attrspec]]
  (fn xproc-elt-v [evt]
    (let [ename (:qname evt)
          eattrs (:attrs evt)
          required (into {} (filter (fn [[_ spec]]
                                      (= spec :required)) attrspec))
          unknown (reduce #(dissoc %1 %2) eattrs (keys attrspec))]
      (and
       ;; the name matches
       (xmlutil/qn-eq qname ename)
       ;; all of the unknown attributes are extension attributes
       (empty? (filter (fn [[attr _]]
                         (not (extension-attr-name? attr))) unknown))
       ;; all required specified
       (empty? (reduce #(dissoc %1 %2) required (keys eattrs)))))))

(defn- xproc-ast-constructor-f 
  [type]
  (fn xproc-ast-c [selt content]
    (let [attrs-grouped (group-by (fn [[attr _]]
                                    (extension-attr-name? attr)) (:attrs selt))
          regular-attrs (into {} (get attrs-grouped false))
          extension-attrs (into {} (get attrs-grouped true))]
      {:type type
       :content content
       :attrs regular-attrs
       :extension-attrs extension-attrs})))

(defmacro defxprocelt
  [var {qname :qname attrs :attrs model-g :model type :type}]
  (if (nil? qname)
    (throw (IllegalArgumentException. "qname is required"))
    (let [ctype (or type `(keyword (xmlutil/local-name ~qname)))
          constructor `(xproc-ast-constructor-f ~ctype)
          elt-validator `(xproc-elt-validator-f ~qname ~attrs)]
      `(xmlast/defelt ~var ~constructor ~model-g ~elt-validator))))

;;; 


(defxprocelt documentation-g {:qname qn-e-documentation 
                              :model #(fp/rep* (xmlast/well-formed-content-g %))})

(defxprocelt pipeinfo-g {:qname qn-e-pipeinfo
                         :model #(fp/rep* (xmlast/well-formed-content-g %))})

(defxprocelt empty-g {:qname qn-e-empty})

(defxprocelt pipe-g {:qname qn-e-pipe
                     :attrs {qn-a-step :required
                             qn-a-port :required}})

(defxprocelt document-g {:qname qn-e-document
                         :attrs {qn-a-href :required}})

(defxprocelt data-g {:qname qn-e-data
                     :attrs {qn-a-href :required
                             qn-a-wrapper :optional
                             qn-a-wrapper-prefix :optional
                             qn-a-wrapper-namespace :optional
                             qn-a-content-type :optional}})

(defxprocelt inline-g {:qname qn-e-inline
                       :attrs {qn-a-exclude-inline-prefixes :optional}
                       :model #(xmlast/well-formed-content-g %)})

(defn- connection-g 
  [ctx]
  (fp/alt (empty-g ctx)
          (document-g ctx)
          (inline-g ctx)
          (data-g ctx)
          (pipe-g ctx)))

(defxprocelt namespaces-g {:qname qn-e-namespaces
                           :attrs {qn-a-binding :optional
                                   qn-a-element :optional
                                   qn-a-except-prefixes :optional}})

(defxprocelt variable-g {:qname qn-e-variable
                         :attrs {qn-a-name :required
                                 qn-a-select :required}
                         :model #(fp/conc (fp/opt (connection-g %))
                                          (fp/rep* (namespaces-g %)))})

(defxprocelt with-option-g {:qname qn-e-with-option
                            :attrs {qn-a-name :required
                                    qn-a-select :required}
                            :model #(fp/conc (fp/opt (connection-g %))
                                             (fp/rep* (namespaces-g %)))})

(defxprocelt with-param-g {:qname qn-e-with-param
                           :attrs {qn-a-name :required
                                   qn-a-select :required
                                   qn-a-port :optional}
                           :model #(fp/conc (fp/opt (connection-g %))
                                            (fp/rep* (namespaces-g %)))})

(defxprocelt option-g {:qname qn-e-option
                       :attrs {qn-a-name :required
                               qn-a-required :optional
                               qn-a-select :required}})

(defxprocelt log-g {:qname qn-e-log
                    :attrs {qn-a-port :required
                            qn-a-href :optional}})

;; TODO atomic steps do not allow connections! - perhaps check while traversing the AST?
(defxprocelt input-g {:qname qn-e-input
                      :attrs {qn-a-port :required
                              qn-a-select :optional}
                      :model #(fp/opt (fp/alt (empty-g %)
                                              (fp/rep+ (fp/alt (pipe-g %)
                                                               (document-g %)
                                                               (inline-g %)
                                                               (data-g %)))))})

(defxprocelt input-decl-g {:qname qn-e-input
                           :attrs {qn-a-port :required
                                   qn-a-sequence :optional
                                   qn-a-primary :optional
                                   qn-a-kind :optional
                                   qn-a-select :optional}
                           :model #(fp/opt (fp/alt (empty-g %)
                                                   (fp/alt (document-g %)
                                                           (inline-g %)
                                                           (data-g %))))})

(defxprocelt output-decl-g {:qname qn-e-output
                            :attrs {qn-a-port :required
                                    qn-a-sequence :optional
                                    qn-a-primary :optional}
                            :model #(fp/opt (fp/alt (empty-g %)
                                                    (fp/alt (pipe-g %)
                                                            (document-g %)
                                                            (inline-g %)
                                                            (data-g %))))})

(defxprocelt iteration-source-g {:qname qn-e-iteration-source
                                 :attrs {qn-a-select :optional}
                                 :model #(fp/opt (fp/alt (empty-g %)
                                                         (fp/alt (pipe-g %)
                                                                 (document-g %)
                                                                 (inline-g %)
                                                                 (data-g %))))})

(declare subpipeline-g)                 ;forward declaration

(defxprocelt for-each-g {:qname qn-e-for-each
                         :attrs {qn-a-name :optional}
                         :model #(fp/conc (fp/opt (iteration-source-g %))
                                          (fp/rep* (fp/alt (output-decl-g %)
                                                           (log-g %)))
                                          (subpipeline-g %))})

(defxprocelt viewport-source-g {:qname qn-e-viewport-source
                                :model #(fp/opt (fp/alt (pipe-g %)
                                                        (document-g %)
                                                        (inline-g %)
                                                        (data-g %)))})

(defxprocelt viewport-g {:qname qn-e-viewport
                         :attrs {qn-a-name :optional
                                 qn-a-match :required}
                         :model #(fp/conc (fp/opt (viewport-source-g %))
                                          (fp/opt (output-decl-g %))
                                          (fp/opt (log-g %))
                                          (subpipeline-g %))})

(defxprocelt xpath-context-g {:qname qn-e-xpath-context
                              :model #(fp/alt (empty-g %)
                                              (pipe-g %)
                                              (document-g %)
                                              (inline-g %)
                                              (data-g %))})

(defxprocelt when-g {:qname qn-e-when
                     :attrs {qn-a-test :required}
                     :model #(fp/conc (fp/opt (xpath-context-g %))
                                      (fp/rep* (fp/alt (output-decl-g %)
                                                       (log-g %)))
                                      (subpipeline-g %))})

(defxprocelt otherwise-g {:qname qn-e-otherwise
                          :model #(fp/conc (fp/rep* (fp/alt (output-decl-g %)
                                                            (log-g %)))
                                           (subpipeline-g %))})

(defxprocelt choose-g {:qname qn-e-choose
                       :attrs {qn-a-name :optional}
                       :model #(fp/conc (fp/opt (xpath-context-g %))
                                        (fp/rep* (variable-g %))
                                        (fp/rep* (when-g %))
                                        (fp/opt (otherwise-g %))
                                        (subpipeline-g %))})

(defxprocelt group-g {:qname qn-e-group
                      :attrs {qn-a-name :optional}
                      :model #(fp/conc (fp/rep* (fp/alt (output-decl-g %)
                                                        (log-g %)))
                                       (subpipeline-g %))})

(defxprocelt catch-g {:qname qn-e-catch
                      :attrs {qn-a-name :optional}
                      :model #(fp/conc (fp/rep* (fp/alt (output-decl-g %)
                                                        (log-g %)))
                                       (subpipeline-g %))})

(defxprocelt try-g {:qname qn-e-try
                    :attrs {qn-a-name :optional}
                    :model #(fp/conc (fp/rep* (variable-g %))
                                     (group-g %)
                                     (catch-g %))})

(xmlast/defelt step-g 
  (xproc-ast-constructor-f :step)
  #(fp/rep* (fp/alt (input-g %)
                    (with-option-g %)
                    (with-param-g %)
                    (log-g %)))
  (fn step-elt-v [evt]
    (not (nil? (xmlutil/ns-uri (:qname evt))))))

(defn- subpipeline-g 
  [ctx]
  (fp/conc (fp/rep* (variable-g ctx))
           (fp/rep+ (fp/alt (for-each-g ctx)
                            (viewport-g ctx)
                            (choose-g ctx)
                            (group-g ctx)
                            (try-g ctx)
                            (step-g ctx)))))

(defxprocelt import-g {:qname qn-e-import
                       :attrs {qn-a-href :required}})

(defxprocelt serialization-g {:qname qn-e-serialization
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

(declare declare-step-g)                ;forward declaration
(defxprocelt pipeline-g {:qname qn-e-pipeline
                         :attrs {qn-a-name :optional
                                 qn-a-type :optional
                                 qn-a-psvi-required :optional
                                 qn-a-xpath-version :optional
                                 qn-a-exclude-inline-prefixes :optional
                                 qn-a-version :optional}
                         :model #(fp/conc (fp/rep* (fp/alt (input-decl-g %)
                                                           (output-decl-g %)
                                                           (option-g %)
                                                           (log-g %)
                                                           (serialization-g %)))
                                          (fp/rep* (fp/alt (declare-step-g %)
                                                           (pipeline-g %)
                                                           (import-g %)))
                                          (subpipeline-g %))})

(defxprocelt declare-step-g {:qname qn-e-declare-step
                             :attrs {qn-a-name :optional
                                     qn-a-type :optional
                                     qn-a-psvi-required :optional
                                     qn-a-xpath-version :optional
                                     qn-a-exclude-inline-prefixes :optional
                                     qn-a-version :optional}
                             :model #(fp/conc (fp/rep* (fp/alt (input-decl-g %)
                                                               (output-decl-g %)
                                                               (option-g %)
                                                               (log-g %)
                                                               (serialization-g %)))
                                              (fp/opt (fp/conc (fp/rep* (fp/alt (declare-step-g %)
                                                                                (pipeline-g %)
                                                                                (import-g %)))
                                                               (subpipeline-g %))))})

(defxprocelt library-g {:qname qn-e-library
                        :attrs {qn-a-psvi-required :optional
                                qn-a-xpath-version :optional
                                qn-a-exclude-inline-prefixes :optional
                                qn-a-version :optional}
                        :model #(fp/rep* (fp/alt (import-g %)
                                                 (declare-step-g %)
                                                 (pipeline-g %)))})

;;; 

(defn import-target-g 
  [ctx]
  (xmlast/opt-doc-g #(fp/alt (pipeline-g %)
                             (declare-step-g %)
                             (library-g %))
                    ctx))

(defn main-pipeline-g 
  [ctx]
  (xmlast/opt-doc-g #(fp/alt (pipeline-g %)
                             (declare-step-g %))
                    ctx))


