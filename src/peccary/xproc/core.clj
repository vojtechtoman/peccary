(ns peccary.xproc.core
  (:gen-class)
  (:require [clojure.java.io :refer :all]
            [peccary.xml.parse :as xmlparse]
            [peccary.xproc.ast :as xprocast]
            [peccary.xproc.error :refer :all]))

(defn parse-pipeline
  [x]
  (with-open [s (input-stream x)]
    (let [evts (xmlparse/parse s)
          steps (xprocast/make-pipeline-step-source evts)
          [type step] (first steps)]
      step                              ;err-XS0059???
      )))
