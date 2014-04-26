(ns peccary.util
  (:gen-class)
  (:require [clojure.zip :as zip])
  (:import [java.net URI]))

(defn resolve-uri
  [parent relative]
  (cond
   (empty? parent) relative
   (empty? relative) parent
   :else (let [parent-uri (URI. parent)
               relative-uri (URI. relative)
               resolved-uri (.resolve parent-uri relative-uri)]
           (.toString resolved-uri))))

;;; 

(defn- apply-editors
  [loc state editors]
  (let [node (zip/node loc)
        ectx (reduce (fn [ctx editor]
                       (let [state (:state ctx)
                             node (:node ctx)]
                         (merge {:state state
                                 :node node}
                                (editor state node))))
                     {:state state
                      :node node}
                     editors)
        estate (:state ectx)
        enode (:node ectx)
        eloc (zip/replace loc enode)]
    {:loc eloc
     :state estate}))

(defn zip-edit
  [z & [initial-state pre-editors post-editors]]
  (loop [loc z
         stack (list initial-state)
         dir :down]
    (if (= :down dir)
      ;; :down -> apply pre-editors and descend
      (let [state (first stack)
            ectx (apply-editors loc state pre-editors)
            eloc (:loc ectx)
            estate (:state ectx)
            down (zip/down eloc)]
        ;; recur with the first child if available, otherwise with the next sibling
        (if (nil? down)
          (recur eloc stack :next)
          (recur down (cons estate stack) :down)))
      ;; :next -> apply the post editors, then proceed with the next sibling
      (let [state (first stack)
            ectx (apply-editors loc state post-editors)
            eloc (:loc ectx)
            estate (:state ectx)
            next (zip/right eloc)]
        ;; recur with next sibling if available, otherwise go up
        (if (nil? next)
          (if-let [parent (zip/up eloc)]
            (recur parent (rest stack) :next) ;go up
            (zip/node eloc))           ;no parent -> we are done
          (recur next (cons estate (rest stack)) :down))))))
