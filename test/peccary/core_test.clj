(ns peccary.core-test
  (:require [midje.sweet :refer :all]
            [clojure.test :refer :all]
            [peccary.xml.parse :as xmlparse]
            [peccary.xml.ast :as xmlast]
            [peccary.xproc.ast :as xprocast]
))

(defn- parse-pipeline
  [pipeline]
  (with-open [f (java.io.FileInputStream. pipeline)]
    (let [parsed (xmlparse/parse f)
          ast (xmlast/parse xprocast/main-pipeline-g parsed)]
      ast)))

;; (fact (parse-pipeline "test/data/identity.xpl")
;;       => {:type :pipeline
;;           :attrs {#peccary.xml.util.QName{:local-name "version" :ns-uri nil :prefix nil} "1.0"}
;;           :extension-attrs {}
;;           :content [{:type :step :attrs {} :extension-attrs {} :content []}]
;;           })

(deftest ast-test
  (testing "Simple XProc AST parsing tests"
    (is (= (parse-pipeline "test/data/identity.xpl")
           {:type :pipeline
            :attrs {#peccary.xml.util.QName{:local-name "version" :ns-uri nil :prefix nil} "1.0"}
            :extension-attrs {}
            :content [{:type :step :attrs {} :extension-attrs {} :content []}]
            }))))
