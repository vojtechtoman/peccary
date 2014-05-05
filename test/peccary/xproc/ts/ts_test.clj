(ns peccary.xproc.ts.ts-test
  (:require [clojure.test :refer :all]
            [peccary.xproc.compile :as xprocc]
            [peccary.testutil :refer :all]
            [peccary.xproc.ts :as ts]))

(def test-suite "test/data/ts/test-suite.xml")

(def declare-step-001 "test/data/ts/required/declare-step-001.xml")

(defn- str->ts-ptree
  [str]
  (str->ptree str ts/evts->ptree))

(defn- file->ts-ptree
  [f]
  (file->ptree f ts/evts->ptree))

;;;

(deftest invalid-test
  (testing "Parsing an invalid XProc test document" ;TODO introduce a real error
    (is (nil? (str->ts-ptree "<t:test xmlns:t='http://xproc.org/ns/testsuite'/>")))))

(deftest valid-test
  (testing "Parsing a valid XProc test document"
    (is (-> declare-step-001 file->ts-ptree))))

(deftest valid-test-suite
  (testing "Parsing a valid XProc test suite document"
    (is (-> test-suite file->ts-ptree))))


