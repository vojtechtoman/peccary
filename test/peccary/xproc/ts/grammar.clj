(ns peccary.xproc.ts.grammar
  (:gen-class)
  (:require [peccary.xml :refer :all]
            [peccary.xml.ast :as xmlast]
            [peccary.xproc.grammar :as xprocg]
            [peccary.xproc.ts.vocabulary :as tsv]
            [name.choi.joshua.fnparse :as fp]))

(defn- create-ts-elt-validator
  [qname & [attrspec]]
  (fn ts-elt-v [evt]
    (let [ename (:qname evt)
          eattrs (:attrs evt)
          required (into {} (filter (fn [[_ spec]]
                                      (= spec :required))
                                    attrspec))]
      (and
       ;; the name matches
       (= qname ename)
       ;; all required specified
       (let [req-unspecified (reduce #(dissoc %1 %2)
                                     required
                                     (keys eattrs))]
         (or (empty? req-unspecified)
             (throw (ex-info (str "Required attribute(s) not specified: " req-unspecified) {}))))))))

(defn- create-ts-ast-constructor
  [type]
  (fn ts-ast-c [ctx selt content]
    (let [attrs (:attrs selt)
          loc (:location selt)
          posname (:posname selt)]
      {:type type
       :content content
       :attrs attrs
       :ctx (assoc ctx :location loc :posname posname)})))

(defmacro deftselt
  [var {qname :qname attrs :attrs model-rf :model type :type}]
  {:pre [(not (nil? qname))]}
  (let [ctype (or type `(keyword (local-name ~qname)))
        constructor `(create-ts-ast-constructor ~ctype)
        elt-validator `(create-ts-elt-validator ~qname ~attrs)]
    `(xmlast/defelt ~var ~constructor ~model-rf ~elt-validator)))

;;; 

(deftselt title-rf {:qname tsv/qn-e-title
                    :model #(fp/rep* (xmlast/well-formed-content-rf %))})


(deftselt description-rf {:qname tsv/qn-e-description
                          :model #(fp/rep* (xmlast/well-formed-content-rf %))})

(deftselt document-rf {:qname tsv/qn-e-document
                       :attrs {tsv/qn-a-href :optional}
                       :model #(fp/opt (xmlast/well-formed-content-rf %))})

(deftselt option-rf {:qname tsv/qn-e-option
                     :attrs {tsv/qn-a-name :required
                             tsv/qn-a-value :required}})

(deftselt parameter-rf {:qname tsv/qn-e-parameter
                        :attrs {tsv/qn-a-port :optional
                                tsv/qn-a-name :required
                                tsv/qn-a-value :required}})

(deftselt input-rf {:qname tsv/qn-e-input
                    :attrs {tsv/qn-a-port :required
                            tsv/qn-a-href :optional}
                    :model #(fp/alt (fp/rep* (document-rf %))
                                    (xmlast/well-formed-content-rf %))})

(deftselt output-rf {:qname tsv/qn-e-output
                     :attrs {tsv/qn-a-port :required}
                     :model #(fp/alt (fp/rep* (document-rf %))
                                     (xmlast/well-formed-content-rf %))})

(deftselt pipeline-rf {:qname tsv/qn-e-pipeline
                       :attrs {tsv/qn-a-href :optional}
                       :model #(fp/opt (fp/alt (xprocg/pipeline-rf %)
                                               (xprocg/declare-step-rf %)))})

(def compare-pipeline-rf pipeline-rf)

;;; Note: the "error" tests have a more strict grammar, but let's ignore that
(deftselt test-rf {:qname tsv/qn-e-test
                   :attrs {qn-xml-id :optional
                           tsv/qn-a-ignore-whitespace-differences :optional
                           tsv/qn-a-error :optional}
                   :model #(fp/conc (title-rf %)
                                    (fp/opt (description-rf %))
                                    (fp/rep* (fp/alt (input-rf %)
                                                     (parameter-rf %)
                                                     (option-rf %)))
                                    (pipeline-rf %)
                                    (fp/opt (compare-pipeline-rf %))
                                    (fp/rep* (output-rf %)))})

(deftselt test-suite-rf {:qname tsv/qn-e-test-suite
                         :attrs {tsv/qn-a-xinclude :optional}
                         :model #(fp/conc (title-rf %)
                                          (fp/rep+ (test-rf %)))})


