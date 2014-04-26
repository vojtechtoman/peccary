(ns peccary.testutil
  (:require [clojure.java.io :as jio]
            [peccary.xml :refer :all]
            [peccary.xml.parse :as xmlparse]))

(defn parse
  [x]
  ;; Note: we explicitly realize the whole evts sequence here
  ;; to avoid 'stream closed' errors
  (with-open [stream (jio/input-stream x)]
    (let [evts (xmlparse/parse stream)
          c (count evts)]
      evts)))

(defn parse-str
  [s]
  (xmlparse/parse-str s))

(defn evts->ptree
  [evts e->p]
  (e->p evts))

(defn file->ptree
  [file e->p]
  (-> file parse e->p))

(defn str->ptree
  [s e->p]
  (-> s parse-str e->p))
