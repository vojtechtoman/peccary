(ns peccary.xproc.ts.ast-test
  (:require [clojure.test :refer :all]
            [peccary.xml :refer :all]
            [peccary.xml.ast :as xmlast]
            [peccary.xml.parse :as xmlparse]
            [peccary.testutil :refer :all]
            [peccary.xproc.ts.grammar :as tsg]
            [peccary.xproc.ts.ast :as tsast]))

(def declare-step-001 "test/data/ts/required/declare-step-001.xml")
(def declare-step-001-str (slurp declare-step-001))


(def declare-step-001-ast
  {:type :test
   :content [{:type :title
              :content [{:data "Test p:declare-step-001"
                         :type :text}]
              :attrs {}}
             {:type :description
              :content [{:type :text
                         :data "\n    "}
                        {:type :start-element
                         :attrs {}
                         :qname #peccary.xml.QName{:local-name "p" :ns-uri "http://www.w3.org/1999/xhtml"}}
                        {:type :text
                         :data "Tests nested p:declare-step"}
                        {:type :end-element
                         :qname #peccary.xml.QName{:local-name "p" :ns-uri "http://www.w3.org/1999/xhtml"}}
                        {:type :text
                         :data "\n  "}]
              :attrs {#peccary.xml.QName{:local-name "xmlns" :ns-uri "http://www.w3.org/2000/xmlns/"} "http://www.w3.org/1999/xhtml"}}
             {:type :input
              :content [{:type :document
                         :content [{:type :text
                                    :data "\n      "}
                                   {:type :start-element
                                    :attrs {#peccary.xml.QName{:local-name "xmlns" :ns-uri "http://www.w3.org/2000/xmlns/"} nil}
                                    :qname #peccary.xml.QName{:local-name "doc"}}
                                   {:type :end-element
                                    :qname #peccary.xml.QName{:local-name "doc":ns-uri nil}}
                                   {:type :text
                                    :data "\n    "}]
                         :attrs {}}
                        {:type :document
                         :content [{:type :text
                                    :data "\n      "}
                                   {:type :start-element
                                    :attrs {#peccary.xml.QName{:local-name "xmlns" :ns-uri "http://www.w3.org/2000/xmlns/"} nil}
                                    :qname #peccary.xml.QName{:local-name "doc"}}
                                   {:type :end-element
                                    :qname #peccary.xml.QName{:local-name "doc"}}
                                   {:type :text
                                    :data "\n    "}]
                         :attrs {}}]
              :attrs {#peccary.xml.QName{:local-name "port"} "source"}}
             {:type :pipeline
              :content [{:type :declare-step
                         :content [{:type :input
                                    :content []
                                    :attrs {#peccary.xml.QName{:local-name "port"} "source"
                                            #peccary.xml.QName{:local-name "sequence"} "true"}
                                    :extension-attrs {}}
                                   {:type :output
                                    :content []
                                    :attrs {#peccary.xml.QName{:local-name "port"} "result"}
                                    :extension-attrs {}}
                                   {:type :declare-step
                                    :content [{:type :input
                                               :content []
                                               :attrs {#peccary.xml.QName{:local-name "port"} "source"
                                                       #peccary.xml.QName{:local-name "sequence"} "true"}
                                               :extension-attrs {}}
                                              {:type :output
                                               :content []
                                               :attrs {#peccary.xml.QName{:local-name "port"} "result"}
                                               :extension-attrs {}}
                                              {:type :step
                                               :step-type #peccary.xml.QName{:local-name "count" :ns-uri "http://www.w3.org/ns/xproc"}
                                               :content []
                                               :attrs {}
                                               :extension-attrs {}}]
                                    :attrs {#peccary.xml.QName{:local-name "type"} "foo:test"}
                                    :extension-attrs {}}
                                   {:type :step
                                    :step-type #peccary.xml.QName{:local-name "test" :ns-uri "http://acme.com/test"}
                                    :content []
                                    :attrs {}
                                    :extension-attrs {}}]
                         :attrs {#peccary.xml.QName{:local-name "version"} "1.0"}
                         :extension-attrs {}}]
              :attrs {}}
             {:type :output
              :content [{:type :start-element
                         :attrs {}
                         :qname #peccary.xml.QName{:local-name "result" :ns-uri "http://www.w3.org/ns/xproc-step"}}
                        {:type :text
                         :data "2"}
                        {:type :end-element
                         :qname #peccary.xml.QName{:local-name "result" :ns-uri "http://www.w3.org/ns/xproc-step"}}]
              :attrs {#peccary.xml.QName{:local-name "port"} "result"}}]
   :attrs {#peccary.xml.QName{:local-name "t" :ns-uri "http://www.w3.org/2000/xmlns/"} "http://xproc.org/ns/testsuite"
           #peccary.xml.QName{:local-name "p" :ns-uri "http://www.w3.org/2000/xmlns/"} "http://www.w3.org/ns/xproc"
           #peccary.xml.QName{:local-name "c" :ns-uri "http://www.w3.org/2000/xmlns/"} "http://www.w3.org/ns/xproc-step"
           #peccary.xml.QName{:local-name "err" :ns-uri "http://www.w3.org/2000/xmlns/"} "http://www.w3.org/ns/xproc-error"}}
)
(defn- str-ts-ast
  [str]
  (str-ast str tsast/make-ts-ast))

(defn- file-ts-ast
  [f]
  (file-ast f tsast/make-ts-ast))

;;;

(deftest invalid-test
  (testing "Parsing an invalid XProc test document" ;TODO introduce a real error
    (is (nil? (str-ts-ast "<t:test xmlns:t='http://xproc.org/ns/testsuite'/>")))))

(deftest valid-test
  (testing "Parsing a valid XProc test document"
    (is (ast-eq (-> declare-step-001 file-ts-ast (strip-key :location)) declare-step-001-ast))))


