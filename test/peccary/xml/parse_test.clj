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
         [{:type :start-document}
          {:type :start-element :qname (qn "doc") :attrs {}}
          {:type :start-element :qname (qn "para") :attrs {(qn "attr") "foo"}}
          {:type :text :data "bar"}
          {:type :end-element :qname (qn "para")}
          {:type :comment :data " comment "}
          {:type :text :data "cdata"}   ;TODO wrong type should be :cdata!
          {:type :pi :data "target" :target "pi"}
          {:type :text :data "baz"}
          {:type :end-element :qname (qn "doc")}
          {:type :end-document}]
         )))

