(ns peccary.xproc.ts.vocabulary
  (:gen-class)
  (:require [peccary.xml :refer :all]))


(def ^:const ns-ts "http://xproc.org/ns/testsuite")

(defn ts-qn
  [local-name & [prefix]]
  (qn local-name ns-ts prefix))

(def ^:const qn-e-test-suite (ts-qn "test-suite"))
(def ^:const qn-e-test (ts-qn "test"))
(def ^:const qn-e-title (ts-qn "title"))
(def ^:const qn-e-description (ts-qn "description"))
(def ^:const qn-e-input (ts-qn "input"))
(def ^:const qn-e-parameter (ts-qn "parameter"))
(def ^:const qn-e-option (ts-qn "option"))
(def ^:const qn-e-pipeline (ts-qn "pipeline"))
(def ^:const qn-e-compare-pipeline (ts-qn "compare-pipeline"))
(def ^:const qn-e-output (ts-qn "output"))
(def ^:const qn-e-document (ts-qn "document"))



(def ^:const qn-a-xinclude (qn "xinclude"))
(def ^:const qn-a-ignore-whitespace-differences (qn "ignore-whitespace-differences"))
(def ^:const qn-a-error (qn "error"))
(def ^:const qn-a-port (qn "port"))
(def ^:const qn-a-href (qn "href"))
(def ^:const qn-a-name (qn "name"))
(def ^:const qn-a-value (qn "value"))


