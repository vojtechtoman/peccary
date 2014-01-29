(ns peccary.util
  (:gen-class)
  (:require [clojure.zip :as zip]))


;;; zipper - the zipper to process
;;; matcher - fn [node] that decides if node needs to be updated
;;; editor - fn [matcher-result node] that produces a modified node
(defn zipper-edit [zipper matcher editor]
  (loop [loc zipper]
    (if (zip/end? loc)
      (zip/root loc)
      (if-let [matcher-result (matcher (zip/node loc))]
        (recur (zip/next (zip/edit loc (partial editor matcher-result))))
        (recur (zip/next loc))))))
