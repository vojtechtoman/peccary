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


;;; A hierarchy of AST node types that represent various relationships between different node types
;;; ::step-declaration (node represents a step declaration
;;;   |_ ::declare-step
;;;        |_ ::pipeline
;;; ::contains-imports (node may contain p:import statements)
;;;   |_ ::library
;;;   |_ ::declare-step
;;;        |_ ::pipeline (because ::pipeline derives from ::declare-step)
;;; ::step-source (node is a source of steps)
;;;   |_ ::library
;;;   |_ ::declare-step
;;;        |_ ::pipeline (because ::pipeline derives from ::declare-step)
(def node-hierarchy
  (-> (make-hierarchy)
      (derive ::pipeline ::declare-step)
      (derive ::declare-step ::step-declaration)
      (derive ::declare-step ::contains-imports)
      (derive ::declare-step ::step-source)
      (derive ::library ::contains-imports)
      (derive ::library ::step-source)))

(defn- node-hierarchy-type
  [node]
  (let [type (:type node)]
    ({:pipeline ::pipeline :declare-step ::declare-step :library ::library} type)))


;;; miscellaneous helper methods

;;; matcher - fn [node] that decides if node needs to be updated
;;; editor - fn [matcher-result node] that produces a modified node
(defn ast-edit [ast matcher editor]
  (let [z (zip/zipper map? :content (fn [n c] (assoc n :content c)) ast)]
    (util/zipper-edit z matcher editor)))

(defn- type-filter
  "Returns a fn that takes a node and returns true if the type of node is equal to type"
  [type]
  (fn [node]
    (= type (:type node))))

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

;;; AST processing phase: filtering based on use-when expressions
(defn- process-use-when 
  [[mr node]] ;TODO
  node)

;;; 

;;; AST processing phase: insertion of implicit input/output ports
(defmulti insert-implicit-ports (fn [_ node] (:type node)))

(defmethod insert-implicit-ports :pipeline [mr node]
  (let [content (:content node)
        source (create-port port-source :input {qn-a-kind "document" qn-a-sequence "true" qn-a-primary "true"})
        parameters (create-port port-parameters :input {qn-a-kind "parameter" qn-a-sequence "true" qn-a-primary "true"})        result (create-port port-result :output {qn-a-sequence "true" qn-a-primary "true"})
                                                         
        new-content (vec (concat [source result parameters] content))]
    (assoc node :content new-content)))

(defmethod insert-implicit-ports :default [_ node]
  node)

;;; 

;;; AST processing phase: manufacturing of connections between unconnected default readable ports
(defmulti insert-default-connections (fn [_ node] (:type node)))
(defmethod insert-default-connections :default [_ node]
  node)

;;; 

;;; AST processing phase: resolution of imports
(defmulti resolve-imports (fn [_ node] (node-hierarchy-type node))) :hierarchy node-hierarchy

(declare parse-import-target)
(defmethod resolve-imports ::contains-imports [mr node]
  (let [content (:content node)
        imports (filter (type-filter :import) content)
        sans-imports (remove (type-filter :import) content)
        steps (reduce (fn [steps import]
                        (let [href (get-attr import qn-a-href)
                              imported-steps (parse-import-target href)]
                          (merge steps imported-steps))) {} imports)] ;TODO duplicates etc.
    (assoc node :content sans-imports :in-scope-types steps)))

(defmethod resolve-imports :default [mr node]
  node)

;;; 

(defn- make-dependency-graph
  [step]
  nil)

;;; 

(defn process-ast
  [ast]
  (let [matcher :type
        phases [process-use-when
                resolve-imports
                insert-implicit-ports
                insert-default-connections]]
    (reduce (fn [ast phase]
              (ast-edit ast matcher phase)) ast phases)))

(defn make-pipeline-ast
  [evts]
  (xmlast/parse main-pipeline-rf evts))

(defn make-import-target-ast
  [evts]
  (xmlast/parse import-target-rf evts))

(defn- make-step-source
  [evts ast-builder]
  (let [ast (ast-builder evts)
        steps (process-ast ast)]
    steps))

(defn make-pipeline-step-source
  [evts]
  (make-step-source evts make-pipeline-ast))

(defn- make-import-target-step-source
  [evts]
  (make-step-source evts make-import-target-ast))

(defn- parse-import-target
  [x]
  (with-open [s (input-stream x)]
    (let [evts (xmlparse/parse s)]
      (make-step-source evts))))
