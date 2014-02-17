(ns peccary.xproc.ast-test
  (:require [clojure.test :refer :all]
            [peccary.xml.parse :as xmlparse]
            [peccary.xml.ast :as xmlast]
            [peccary.xproc.ast :as xprocast]
            [peccary.xproc.grammar :as xprocg]))

(def identity-file "test/data/identity.xpl")
(def identity-str (slurp identity-file))
(def identity-ast {:type :pipeline,
                   :attrs {#peccary.xml.util.QName{:local-name "version" :ns-uri nil} "1.0"}
                   :extension-attrs {}
                   :content [{:type :step
                              :step-type #peccary.xml.util.QName{:local-name "identity" :ns-uri "http://www.w3.org/ns/xproc"}
                              :content [] :attrs {} :extension-attrs {} }]})

(def identity-ast-processed
  {:type :pipeline
   :attrs {#peccary.xml.util.QName{:local-name "version" :ns-uri nil} "1.0"}
   :extension-attrs {}
   :in-scope-types {}
   :content [{:type :output
              :attrs {#peccary.xml.util.QName{:local-name "port" :ns-uri nil} "result"
                      #peccary.xml.util.QName{:local-name "kind" :ns-uri nil} nil
                      #peccary.xml.util.QName{:local-name "sequence" :ns-uri nil} "true"
                      #peccary.xml.util.QName{:local-name "primary" :ns-uri nil} "true"}}
             {:type :input
              :attrs {#peccary.xml.util.QName{:local-name "port" :ns-uri nil} "parameters"
                      #peccary.xml.util.QName{:local-name "kind" :ns-uri nil} "parameter"
                      #peccary.xml.util.QName{:local-name "sequence" :ns-uri nil} "true"
                      #peccary.xml.util.QName{:local-name "primary" :ns-uri nil} "true"}}
             {:type :input
              :attrs {#peccary.xml.util.QName{:local-name "port" :ns-uri nil} "source"
                      #peccary.xml.util.QName{:local-name "kind" :ns-uri nil} "document"
                      #peccary.xml.util.QName{:local-name "sequence" :ns-uri nil} "true"
                      #peccary.xml.util.QName{:local-name "primary" :ns-uri nil} "true"}}
             {:type :step
              :step-type #peccary.xml.util.QName{:local-name "identity" :ns-uri "http://www.w3.org/ns/xproc"}
              :attrs {}
              :extension-attrs {}
              :content []}]}
  )

                                        ;"http://www.w3.org/ns/xproc"
(defn parse-file
  [file]
  ;; Note: we explicitly realize the whole evts sequence here
  ;; to avoid 'stream closed' errors
  (with-open [f (java.io.FileInputStream. file)]
    (let [evts (xmlparse/parse f)
          c (count evts)]
      evts)))

(defn- parse-str
  [s]
  (xmlparse/parse-str s))

;;; removes the location info from the AST tree to simplify AST comparisons
(defn- strip-location-info
  [ast]
  (xprocast/ast-edit ast
                     nil
                     [(fn strip-location [node state]
                        {:node (dissoc node :location) :state state})]))

(defn make-ast
  [evts]
  (xmlast/parse xprocg/main-pipeline-rf evts))

(defn- file-ast
  [file]
  (-> file parse-file make-ast))

(defn- str-ast
  [s]
  (-> s parse-str make-ast))

(defn- with-ignorable-whitespace
  [s]
  (-> s (clojure.string/replace "<p:" "  <!-- comment --> 
<?a b?>  <p:")
      (clojure.string/replace "/>" "  /> <?a b?> 
<!-- comment --> <!-- comment -->  ")))

(defn- ast-eq
  [ast1 ast2]
  (= (strip-location-info ast1) (strip-location-info ast2)))

;;;

(deftest parse-error
  (testing "Parsing of ill-formed XML"
    (is (thrown? javax.xml.stream.XMLStreamException (str-ast "<a>"))))
  (testing "Parsing of invalid XProc source" ;TODO introduce a real error
    (is (nil? (str-ast "<p:pipeline xmlns:p='http://www.w3.org/ns/xproc' version='1.0'/>")))))

(deftest basic-ast-construction
  (deftest file-and-str-ast-identical
    (testing "The results are the same if we parse a file or a string"
      (are [ast] (ast-eq ast identity-ast)
           (file-ast identity-file)
           (str-ast identity-str))))
  
  (deftest ignorable-content-gets-ignored
    (testing "Ignorable really does get ignored"
      (are [str] (ast-eq (str-ast str) identity-ast)
           identity-str
           (with-ignorable-whitespace identity-str))))) 

;;; 

(deftest ast-manipulations
  (deftest identity-processing
    (testing "AST processing of the identity pipeline"
      (is (ast-eq (xprocast/process-ast identity-ast) identity-ast-processed)))))
