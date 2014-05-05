(ns peccary.xproc.compile-test
  (:require [clojure.test :refer :all]
            [peccary.testutil :refer :all]
            [peccary.util :as util]
            [peccary.xml :refer :all]
            [peccary.xml.grammar :as xmlg]
            [peccary.xml.parse :as xmlparse]
            [peccary.xproc.compile :as xprocc]
            [peccary.xproc.grammar :as xprocg]
            [peccary.xproc.vocabulary :as xprocv]))

(def identity-file "test/data/identity.xpl")
(def identity-str (slurp identity-file))
;; (def identity-ptree {:type :pipeline
;;                    :attrs {(qn "version") "1.0"}
;;                    :extension-attrs {}
;;                    :content [{:type :step
;;                               :step-type (qn "identity" "http://www.w3.org/ns/xproc")
;;                               :content [] :attrs {} :extension-attrs {} }]})

(def identity-ptree {:type :pipeline
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

(def identity-ptree-processed
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


(defn- str->xproc-ptree
  [str]
  (str->ptree str xprocc/evts->pipeline-ptree))

(defn- file->xproc-ptree
  [f]
  (file->ptree f xprocc/evts->pipeline-ptree))

(defn- with-ignorable-whitespace
  [s]
  (-> s (clojure.string/replace "<p:" "  <!-- comment --> 
<?a b?>  <p:")
      (clojure.string/replace "/>" "  /> <?a b?> 
<!-- comment --> <!-- comment -->  ")))

;;; 

(defn- strip-zptree-key
  [z key]
  (util/zip-edit z
                 nil
                 [(fn strip-k [state node]
                    {:node (dissoc node key) :state state})]))

(defn- strip-zptree-ctx
  [z]
  (strip-zptree-key z :ctx))

(defn- zptree-eq
  [z1 z2]
  (= (strip-zptree-ctx z1) (strip-zptree-ctx z2)))

(defn- ptree-eq
  [t1 t2]
  (let [z1 (xprocc/ptree->zipper t1)
        z2 (xprocc/ptree->zipper t2)]
    (zptree-eq z1 z2)))

;;;

(deftest invalid-xproc
  (testing "Parsing of invalid XProc source" ;TODO introduce a real error
    (is (nil? (str->xproc-ptree "<p:pipeline xmlns:p='http://www.w3.org/ns/xproc' version='1.0'/>")))))

(deftest basic-ptree-construction
  (testing "The results are the same if we parse a file or a string"
    (are [ptree] (ptree-eq ptree identity-ptree)
         (file->xproc-ptree identity-file)
         (str->xproc-ptree identity-str)))
  
  (deftest ignorable-content-gets-ignored
    (testing "Ignorable really does get ignored"
      (are [str] (ptree-eq (str->xproc-ptree str) identity-ptree)
           identity-str
           (with-ignorable-whitespace identity-str))))) 

;;; 

(deftest ptree-manipulations
  (testing "ptree processing of the identity pipeline"
    (is (ptree-eq (xprocc/process-ptree identity-ptree) identity-ptree-processed))))

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
    (are [str] (thrown-xproc-error? "XS0002" (let [ptree (str->xproc-ptree str)]
                                               (xprocc/process-ptree ptree)))
         "<p:pipeline xmlns:p='http://www.w3.org/ns/xproc' version='1.0'>
            <p:identity name='dup'/>
            <p:identity name='dup'/>
          </p:pipeline>"

         "<p:pipeline name='dup' xmlns:p='http://www.w3.org/ns/xproc' version='1.0'>
            <p:identity name='dup'/>
          </p:pipeline>"
         )))


