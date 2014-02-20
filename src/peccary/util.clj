(ns peccary.util
  (:gen-class))

(defn resolve-uri
  [parent relative]
  (cond
   (nil? parent) relative
   (nil? relative) parent
   :else relative))                     ;TODO!
