(ns peccary.xproc.ast
  (:gen-class)
  (:require [clojure.java.io :refer :all]
            [clojure.zip :as zip]
            [peccary.util :as util]
            [peccary.xml.ast :as xmlast]
            [peccary.xml.parse :as xmlparse]
            [peccary.xml.util :as xmlutil]
            [peccary.xproc.error :refer :all]
            [peccary.xproc.grammar :refer :all]
            [peccary.xproc.vocabulary :refer :all]
            [name.choi.joshua.fnparse :as fp]))


;;; AST processing

;;; ast - AST node to process
;;; matcher - fn [node] that decides if node needs to be updated
;;; editor - fn [matcher-result node] that produces a modified node
(defn ast-edit [ast matcher editor]
  (let [z (zip/zipper map? :content (fn [n c] (assoc n :content c)) ast)]
    (util/zipper-edit z matcher editor)))

(defn- type-dispatch
  [node]
  (:type node))

;;; miscellaneous helper methods

(defn process-content
  [node f]
  (when-let [content (:content node)]
    (f content)))

(defn filter-content
  [node f]
  (process-content node (partial filter f)))

(defn- create-port
  [name type & [{kind qn-a-kind sequence qn-a-sequence primary qn-a-primary} opts]]
  (let [attrs (reduce (fn [m [k v]]
                        (if (nil? v) m (assoc m k v)))
                      {qn-a-port name qn-a-kind kind qn-a-sequence sequence qn-a-primary primary})]
    {:type type :attrs attrs}))

(defn- get-attr
  [node attr]
  (-> node :attrs attr))


;;; 


(defmulti insert-input-ports (fn [_ node] (type-dispatch node)))

(defmethod insert-input-ports :pipeline [mr node]
  (let [content (:content node)
        source (create-port port-source :input {qn-a-kind "document" qn-a-sequence "true" qn-a-primary "true"})
        parameters (create-port port-parameters :input {qn-a-kind "parameter" qn-a-sequence "true" qn-a-primary "true"})        result (create-port port-result :output {qn-a-sequence "true" qn-a-primary "true"})
                                                         
        new-content (vec (concat [source result parameters] content))]
    (assoc node :content new-content)))

(defmethod insert-input-ports :default [_ node]
  node)

;;; 

(declare make-step-source)

(defn parse-import-target
  [x]
  (with-open [s (input-stream x)]
    (let [evts (xmlparse/parse s)
          ast (xmlast/parse import-target-rf evts)
          steps (make-step-source ast)]
      steps)))


(defmulti resolve-imports (fn [_ node] (type-dispatch node)))

(defmethod resolve-imports :import [mr node]
  (let [href (get-attr node qn-a-href)
        steps (parse-import-target href)]
    {:type :step-source :content steps}))

(defmethod resolve-imports :default [mr node]
  node)

;;; 

(defmulti process-use-when (fn [_ node] (type-dispatch node)))
(defmethod process-use-when :default [mr node] ;TODO
  node)



;;; 

(defn process-ast
  [ast]
  (let [matcher type-dispatch
        phases [process-use-when
                resolve-imports
                insert-input-ports]]
    (reduce (fn [ast phase]
              (ast-edit ast matcher phase)) ast phases)))

(defn make-step-source
  [ast]
  (process-ast ast))
