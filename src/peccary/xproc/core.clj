(ns peccary.xproc.core
  (:gen-class)
  (:require [clojure.java.io :refer :all]
            [peccary.xml.ast :as xmlast]
            [peccary.xml.parse :as xmlparse]
            [peccary.xproc.ast :as xprocast]
            [peccary.xproc.error :refer :all]
            [peccary.xproc.grammar :as xprocg]))

(defn- make-ast
  [evts]
  (let [ast (xmlast/parse xprocg/main-pipeline-rf evts)]
    ast))

(defn parse-step-source
  [x]
  (with-open [s (input-stream x)]
    (let [evts (xmlparse/parse s)
          ast (make-ast evts)
          steps (xprocast/make-step-source ast)]
      steps)))

(defn parse-pipeline
  [x]
  (let [steps (parse-step-source x)]
    (if (= 1 (count steps))
      (let [[type step] (first steps)]
        step)
      err-XS0059)))

