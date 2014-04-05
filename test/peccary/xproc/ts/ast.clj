(ns peccary.xproc.ts.ast
  (:gen-class)
  (:require [peccary.xml :refer :all]
            [peccary.xml.ast :as xmlast]
            [peccary.xproc.ts.grammar :refer :all]))

(defn make-test-ast
  [evts]
  (xmlast/parse test-rf evts))
