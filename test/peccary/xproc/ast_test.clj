(ns peccary.xproc.ast-test
  (:require [clojure.test :refer :all]
            [peccary.testutil :refer :all]
            [peccary.xml :refer :all]
            [peccary.xml.ast :as xmlast]
            [peccary.xml.parse :as xmlparse]
            [peccary.xproc.ast :as xprocast]
            [peccary.xproc.grammar :as xprocg]
            [peccary.xproc.vocabulary :as xprocv]))

(def identity-file "test/data/identity.xpl")
(def identity-str (slurp identity-file))
;; (def identity-ast {:type :pipeline
;;                    :attrs {(qn "version") "1.0"}
;;                    :extension-attrs {}
;;                    :content [{:type :step
;;                               :step-type (qn "identity" "http://www.w3.org/ns/xproc")
;;                               :content [] :attrs {} :extension-attrs {} }]})

(def identity-ast {:type :pipeline
        :attrs {(qn "version") "1.0"}
        :extension-attrs {}
        :ctx {:posname "!1"
              :location {:offset 103 :column 64 :line 3 :resource nil}
              :base-uri nil
              :ns-context {"p" "http://www.w3.org/ns/xproc"}}
        :content [{:step-type (qn "identity" "http://www.w3.org/ns/xproc")
                   :type :step
                   :content []
                   :attrs {}
                   :extension-attrs {}
                   :ctx {:posname "!1.1"
                         :location {:offset 119 :column 16 :line 4 :resource nil}
                         :base-uri nil
                         :ns-context {"p" "http://www.w3.org/ns/xproc"}}}]})

(def identity-ast-processed
  {:in-scope-types
   {(qn "identity" "http://www.w3.org/ns/xproc")
    {:type
     (qn "identity" "http://www.w3.org/ns/xproc")
     :signature
     {:content [{:type :input
                 :attrs {(qn "port") "source"
                         (qn "kind") "document"
                         (qn "sequence") "true"
                         (qn "primary") "true"}}
                {:type :output
                 :attrs {(qn "port") "result"
                         (qn "kind") nil
                         (qn "sequence") "true"
                         (qn "primary") "true"}}]}
     :body 1}}
   :type :pipeline
   :attrs {(qn "version") "1.0"}
   :extension-attrs {}
   :content [{:type :input
              :attrs {(qn "port") "source"
                      (qn "kind") "document"
                      (qn "sequence") "true"
                      (qn "primary") "true"}}
             {:type :input
              :attrs {(qn "port") "parameters"
                      (qn "kind") "parameter"
                      (qn "sequence") "true"
                      (qn "primary") "true"}}
             {:type :output
              :attrs {(qn "port") "result"
                      (qn "kind") nil
                      (qn "sequence") "true"
                      (qn "primary") "true"}
              :content [{:type :pipe
                         :attrs {(qn "step") "!1.1"
                                 (qn "port") "result"}}]}
             {:type :step
              :step-type (qn "identity" "http://www.w3.org/ns/xproc")
              :attrs {}
              :extension-attrs {}
              :content [{:type :input
                         :attrs {(qn "port") "source"
                                 (qn "kind") "document"
                                 (qn "sequence") "true"
                                 (qn "primary") "true"}
                         :content [{:type :pipe
                                    :attrs {(qn "step") "!1"
                                            (qn "port") "source"}}]}
                        {:type :output
                         :attrs {(qn "port") "result"
                                 (qn "kind") nil
                                 (qn "sequence") "true"
                                 (qn "primary") "true"}}]}]})


(defn- make-xproc-ast
  [evts]
  (xprocg/parse xprocg/main-pipeline-rf evts))

(defn- str-xproc-ast
  [str]
  (str-ast str make-xproc-ast))

(defn- file-xproc-ast
  [f]
  (file-ast f make-xproc-ast))

(defn- with-ignorable-whitespace
  [s]
  (-> s (clojure.string/replace "<p:" "  <!-- comment --> 
<?a b?>  <p:")
      (clojure.string/replace "/>" "  /> <?a b?> 
<!-- comment --> <!-- comment -->  ")))

;;;

(deftest invalid-xproc
  (testing "Parsing of invalid XProc source" ;TODO introduce a real error
    (is (nil? (str-xproc-ast "<p:pipeline xmlns:p='http://www.w3.org/ns/xproc' version='1.0'/>")))))

(deftest basic-ast-construction
  (testing "The results are the same if we parse a file or a string"
    (are [ast] (ast-eq ast identity-ast)
         (file-xproc-ast identity-file)
         (str-xproc-ast identity-str)))
  
  (deftest ignorable-content-gets-ignored
    (testing "Ignorable really does get ignored"
      (are [str] (ast-eq (str-xproc-ast str) identity-ast)
           identity-str
           (with-ignorable-whitespace identity-str))))) 

;;; 

(deftest ast-manipulations
  (testing "AST processing of the identity pipeline"
    (is (ast-eq (xprocast/process-ast identity-ast) identity-ast-processed))))

;;; 

(defmacro thrown-xproc-error?
  [exp-code body]
  `(try
     (do
       ~body
       false)
     (catch clojure.lang.ExceptionInfo e# (let [xdata# (ex-data e#)
                                                type# (:type xdata#)
                                                code# (:code xdata#)]
                                            (and (= :xproc-exception type#)
                                                 (= (xprocv/xproc-error-qn ~exp-code) code#))))))


(deftest static-errors
  (testing "Multiple steps with the same name in the same scope"
    (are [str] (thrown-xproc-error? "XS0002" (let [ast (str-xproc-ast str)]
                                               (xprocast/process-ast ast)))
         "<p:pipeline xmlns:p='http://www.w3.org/ns/xproc' version='1.0'>
            <p:identity name='dup'/>
            <p:identity name='dup'/>
          </p:pipeline>"

         "<p:pipeline name='dup' xmlns:p='http://www.w3.org/ns/xproc' version='1.0'>
            <p:identity name='dup'/>
          </p:pipeline>"
         )))


