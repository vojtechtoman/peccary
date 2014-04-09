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


(defn- make-test-ast
  [evts]
  (xmlast/parse tsg/test-rf evts))

(defn- str-test-ast
  [str]
  (str-ast str make-test-ast))

(defn- file-test-ast
  [f]
  (file-ast f make-test-ast))

;;;

(deftest invalid-test
  (testing "Parsing an invalid XProc test document" ;TODO introduce a real error
    (is (nil? (str-test-ast "<t:test xmlns:t='http://xproc.org/ns/testsuite'/>")))))




