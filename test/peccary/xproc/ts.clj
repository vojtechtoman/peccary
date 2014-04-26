(ns peccary.xproc.ts
  (:gen-class)
  (:require [clojure.java.io :refer :all]
            [clojure.zip :as zip]
            [name.choi.joshua.fnparse :as fp]
            [peccary.xml :refer :all]
            [peccary.xml.ast :as xmlast]
            [peccary.xml.parse :as xmlparse]
            [peccary.xproc.grammar :as xprocg]))

;;; vocabulary

(def ^:private ^:const ns-ts "http://xproc.org/ns/testsuite")

(defn- ts-qn
  [local-name & [prefix]]
  (qn local-name ns-ts prefix))

(def ^:private ^:const qn-e-test-suite (ts-qn "test-suite"))
(def ^:private ^:const qn-e-test (ts-qn "test"))
(def ^:private ^:const qn-e-title (ts-qn "title"))
(def ^:private ^:const qn-e-description (ts-qn "description"))
(def ^:private ^:const qn-e-input (ts-qn "input"))
(def ^:private ^:const qn-e-parameter (ts-qn "parameter"))
(def ^:private ^:const qn-e-option (ts-qn "option"))
(def ^:private ^:const qn-e-pipeline (ts-qn "pipeline"))
(def ^:private ^:const qn-e-compare-pipeline (ts-qn "compare-pipeline"))
(def ^:private ^:const qn-e-output (ts-qn "output"))
(def ^:private ^:const qn-e-document (ts-qn "document"))

(def ^:private ^:const qn-a-xinclude (qn "xinclude"))
(def ^:private ^:const qn-a-ignore-whitespace-differences (qn "ignore-whitespace-differences"))
(def ^:private ^:const qn-a-error (qn "error"))
(def ^:private ^:const qn-a-port (qn "port"))
(def ^:private ^:const qn-a-href (qn "href"))
(def ^:private ^:const qn-a-name (qn "name"))
(def ^:private ^:const qn-a-value (qn "value"))

;;; FnParse/xmlast rules

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

(deftselt title-rf {:qname qn-e-title
                    :model #(fp/opt (xmlast/text-rf %))})

(deftselt description-rf {:qname qn-e-description
                          :model #(fp/rep* (xmlast/well-formed-content-rf %))})

(deftselt document-rf {:qname qn-e-document
                       :attrs {qn-a-href :optional}
                       :model #(fp/opt (xmlast/well-formed-content-rf %))})

(deftselt option-rf {:qname qn-e-option
                     :attrs {qn-a-name :required
                             qn-a-value :required}})

(deftselt parameter-rf {:qname qn-e-parameter
                        :attrs {qn-a-port :optional
                                qn-a-name :required
                                qn-a-value :required}})

(deftselt input-docs-rf {:qname qn-e-input
                    :attrs {qn-a-port :required
                            qn-a-href :optional}
                         :model #(fp/rep* (document-rf %))})

(deftselt input-raw-rf {:qname qn-e-input
                        :attrs {qn-a-port :required
                                qn-a-href :optional}
                        :model #(xmlast/well-formed-content-rf %)})

(defn- input-rf
  [ctx]
  (fp/alt (input-docs-rf ctx)
          (input-raw-rf ctx)))

(deftselt output-docs-rf {:qname qn-e-output
                          :attrs {qn-a-port :required}
                          :model #(fp/rep* (document-rf %))})

(deftselt output-raw-rf {:qname qn-e-output
                         :attrs {qn-a-port :required}
                         :model #(xmlast/well-formed-content-rf %)})

(defn- output-rf
  [ctx]
  (fp/alt (output-docs-rf ctx)
          (output-raw-rf ctx)))

(deftselt pipeline-rf {:qname qn-e-pipeline
                       :attrs {qn-a-href :optional}
                       :model #(fp/opt (fp/alt (xprocg/pipeline-rf %)
                                               (xprocg/declare-step-rf %)))})

(def compare-pipeline-rf pipeline-rf)

;;; Note: the "error" tests have a more strict grammar, but let's ignore that
(deftselt test-rf {:qname qn-e-test
                   :attrs {qn-xml-id :optional
                           qn-a-ignore-whitespace-differences :optional
                           qn-a-error :optional}
                   :model #(fp/conc (title-rf %)
                                    (fp/opt (description-rf %))
                                    (fp/rep* (fp/alt (input-rf %)
                                                     (parameter-rf %)
                                                     (option-rf %)))
                                    (pipeline-rf %)
                                    (fp/opt (compare-pipeline-rf %))
                                    (fp/rep* (output-rf %)))})

(deftselt test-href-rf {:qname qn-e-test
                        :attrs {qn-a-href :required}})

(deftselt test-suite-rf {:qname qn-e-test-suite
                         :attrs {qn-a-xinclude :optional}
                         :model #(fp/conc (title-rf %)
                                          (fp/rep+ (fp/alt (test-rf %)
                                                           (test-href-rf %))))})

(defn- main-ts-rf 
  [ctx]
  (xmlast/opt-doc-rf #(fp/alt (test-rf %)
                              (test-suite-rf %))
                     ctx))

;;; actual test(suite) execution logic 


(defn- attrs
  [n]
  (:attrs n))

(defn- get-attr
  [n attr]
  (-> n attrs (get attr)))

(defn- ctype-first
  [type n]
  (->> n
       (xmlast/cfilter (fn [n]
                         (= type (:type n))))
       first))

(defn- title
  [n]
  (if-let [text (->> n
                     (ctype-first :title)
                     (ctype-first :text)
                     :data)]
    text
    "[no title]"))

(defn- log-title
  [n]
  (let [t (title n)]
    (println t)))

(defn- ztype
  [z]
  (let [n (zip/node z)]
    (:type n)))

(defmulti zrun ztype)

(declare run)
(defmethod zrun :test
  [z]
  (let [n (zip/node z)]
    (if-let [href (get-attr n qn-a-href)]
      (run href)
      (log-title n))))

(defmethod zrun :test-suite
  [z]
  (let [n (zip/node z)]
    (do (log-title n)
        (loop [loc (zip/down z)]
          (when loc
            (do (zrun loc)
                (recur (zip/right loc))))))))

(defmethod zrun :default
  [z])


;;; 

(defn make-ts-ast
  [evts]
  (xmlast/parse main-ts-rf evts))

(defn- parse-ts
  [x]
  (with-open [s (input-stream x)]
    (let [evts (xmlparse/parse s)]
      (make-ts-ast evts))))

(defn run
  [x]
  (let [ast (parse-ts x)
        z (xmlast/make-ast-zipper ast)]
    (zrun z)))
