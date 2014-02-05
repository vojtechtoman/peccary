(ns peccary.xproc.ast-test
  (:require [clojure.test :refer :all]
            [peccary.xml.parse :as xmlparse]
            [peccary.xml.ast :as xmlast]
            [peccary.xproc.ast :as xprocast]
            [peccary.xproc.grammar :as xprocg]))

(def identity-file "test/data/identity.xpl")
(def identity-str (slurp identity-file))
(def identity-ast {:type :pipeline
                   :attrs {#peccary.xml.util.QName{:local-name "version" :ns-uri nil} "1.0"}
                   :extension-attrs {}
                   :content [{:type :step :attrs {} :extension-attrs {} :content []}]})

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
(defn- locationless
  [ast]
  (xprocast/ast-edit ast
                      (fn [n]
                        (:location n))
                      (fn [_ n]
                        (dissoc n :location))))

(defn make-ast
  [evts]
  (let [ast (xmlast/parse xprocg/main-pipeline-rf evts)]
    (locationless ast)))

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

;;;

(deftest parse-error
  (testing "Parsing of ill-formed XML"
    (is (thrown? javax.xml.stream.XMLStreamException (str-ast "<a>"))))
  (testing "Parsing of invalid XProc source" ;TODO introduce a real error
    (is (nil? (str-ast "<p:pipeline xmlns:p='http://www.w3.org/ns/xproc' version='1.0'/>")))))

(deftest file-and-str-ast
  (testing "The results are the same if we parse a file or a string"
    (are [ast] (= ast identity-ast)
         (file-ast identity-file)
         (str-ast identity-str))))

(deftest ignorable-content
  (testing "Ignorable really does get ignored"
    (are [str] (= (str-ast str) identity-ast)
         identity-str
         (with-ignorable-whitespace identity-str))))

;; (deftest process-ast
;;   (testing 
;;     (is (= (-> identity-str str-ast xprocast/process-ast) identity-ast)
;;          )))
