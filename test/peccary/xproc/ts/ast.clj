(ns peccary.xproc.ts.ast
  (:gen-class)
  (:require [peccary.xml :refer :all]
            [peccary.xml.ast :as xmlast]
            [peccary.xproc.ts.grammar :refer :all]))

(defn make-ts-ast
  [evts]
  (xmlast/parse main-ts-rf evts))
