(ns peccary.util
  (:gen-class)
  (:require [clojure.zip :as zip]))


;;; zipper - the zipper to process
;;; matcher - fn [node] that decides if node needs to be updated
;;; editor - fn [matcher-result loc] that produces a modified node
(defn zipper-raw-edit [zipper editor & [matcher]]
  (loop [loc zipper]
    (if (zip/end? loc)
      (zip/root loc)
      (if-let [matcher-result (or (nil? matcher) (matcher (zip/node loc)))]
        (recur (zip/next (zip/edit loc (partial (fn [mr _]
                                                  (editor mr loc)) matcher-result))))
        (recur (zip/next loc))))))

;;; zipper - the zipper to process
;;; matcher - fn [node] that decides if node needs to be updated
;;; editor - fn [matcher-result node] that produces a modified node
(defn zipper-edit [zipper editor & [matcher]]
  (zipper-raw-edit zipper (fn [mr loc]
                            (editor mr (zip/node loc))) matcher))
