(ns peccary.xml.parse-test
  (:require [clojure.test :refer :all]
            [peccary.xml :refer :all]
            [peccary.xml.parse :as xmlparse])
  (:import (javax.xml.stream XMLStreamException)))

(defn parse-file
  [file]
  ;; Note: we explicitly realize the whole evts sequence here
  ;; to avoid 'stream closed' errors
  (with-open [f (java.io.FileInputStream. file)]
    (let [evts (xmlparse/parse f)
          c (count evts)]
      evts)))

(defn parse-str
  [s]
  (xmlparse/parse-str s))

(defn strip-location-info
  [evts]
  (map (fn [e] (dissoc e :location)) evts))

(defn evts-eq
  [evts1 evts2]
  (= (strip-location-info evts1) (strip-location-info evts2)))

;;;

(deftest parse-error
  (testing "Parsing of ill-formed XML"
    (is (thrown? XMLStreamException (doall (parse-str "<a>"))))
    (is (thrown? XMLStreamException (doall (parse-str "<undeclared:a/>"))))
    (is (thrown? XMLStreamException (doall (parse-str "<a><!--</a>"))))
    (is (thrown? XMLStreamException (doall (parse-str "<a b/>"))))
    (is (thrown? XMLStreamException (doall (parse-str "<a undeclared:b='c'/>"))))
    (is (thrown? XMLStreamException (doall (parse-str "<a xmlns:p=''/>"))))
    (is (thrown? XMLStreamException (doall (parse-str "<a xmlns:xmlns='x'/>"))))
    (is (thrown? XMLStreamException (doall (parse-str "<a xmlns:a='b' xmlns:a='b'/>"))))
    (is (thrown? XMLStreamException (doall (parse-str "<?xml version='2.0'?><a/>"))))))

(deftest parse
  (testing "Various examples of well-formed XML documents"
    (are [str exp] (evts-eq (parse-str str) exp)
         "<doc><para attr='foo'>bar</para><!-- comment --><![CDATA[cdata]]><?pi target?>baz</doc>"
         [{:type :start-document :base-uri nil :lang nil}
          {:type :start-element :qname (qn "doc") :attrs {} :base-uri nil :lang nil}
          {:type :start-element :qname (qn "para") :attrs {(qn "attr") "foo"} :base-uri nil :lang nil}
          {:type :text :data "bar" :base-uri nil :lang nil}
          {:type :end-element :qname (qn "para") :base-uri nil :lang nil}
          {:type :comment :data " comment " :base-uri nil :lang nil}
          {:type :text :data "cdata" :base-uri nil :lang nil}   ;TODO wrong type should be :cdata!
          {:type :pi :data "target" :target "pi" :base-uri nil :lang nil}
          {:type :text :data "baz" :base-uri nil :lang nil}
          {:type :end-element :qname (qn "doc") :base-uri nil :lang nil}
          {:type :end-document :base-uri nil :lang nil}]
         )))

