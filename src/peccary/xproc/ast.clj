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
      (derive ::library ::contains-imports)

      (derive ::declare-step ::step-source)
      (derive ::library ::step-source)

      (derive ::atomic-step ::step)
      (derive ::compound-step ::step)
      (derive ::declare-step ::compound-step)
))

(defn- node-hierarchy-type
  [node]
  (let [type (:type node)]
    ({:pipeline ::pipeline :declare-step ::declare-step :library ::library :step ::atomic-step} type)))


;;; miscellaneous helper methods

(defn- make-ast-zipper
  [ast]
  (let []
    (zip/zipper map? :content (fn [n c] (assoc n :content c)) ast)))

(defn- h-matcher
  [& [type]]
  (if type
    (fn [node]
;      (prn 555555555 node (node-hierarchy-type node) (isa? node-hierarchy (node-hierarchy-type node) type))
      (isa? node-hierarchy (node-hierarchy-type node) type))
    nil))


(defmulti get-readable-ports node-hierarchy-type)
(defmethod get-readable-ports ::compound-step
  [node]
  {})                                   ;TODO

(defmethod get-readable-ports :default
  [node]
  nil)


;;; 

(defmulti get-in-scope-types node-hierarchy-type)
(defmethod get-in-scope-types ::compound-step
  [node]
  {})                                   ;TODO

(defmethod get-in-scope-types :default
  [node]
  nil)


;;; 

(defmulti make-state (fn [node parent-state]
                       (node-hierarchy-type node)))

(defmethod make-state ::compound-step
  [node parent-state]
  (let [readable-ports (get-readable-ports node)
        in-scope-types (get-in-scope-types node)]
    (-> parent-state
        (update-in [:readable-ports] #(merge % readable-ports))
        (update-in [:in-scope-types] #(merge % in-scope-types)))))

(defmethod make-state :default
  [loc node parent-state]
  parent-state)


;;; matcher - fn [state node] that decides if node needs to be updated
;;; editor - fn [matcher-result state node] that produces a modified node
;; (defn ast-edit
;;   [ast editor & [matcher]]
;;   (let [z (make-ast-zipper ast)
;;         initial-state nil]
;;     (loop [loc z
;;            s initial-state]
;;       (if (zip/end? loc)
;;         (zip/root loc)
;;         (let [node (zip/node loc)
;;               state (make-state node initial-state)]
;;           (if-let [mr (when matcher (matcher state node))]
;;             (recur (zip/next (zip/edit loc (partial (editor mr state)))) state)
;;             (recur (zip/next loc) state)))))))
;; ;; (defn ast-edit [ast editor & [matcher]]
;; ;;   (let [z (make-ast-zipper ast)]
;; ;;     (util/zipper-edit z editor matcher)))

;;; editor - fn [matcher-result state node] that produces a modified node
;;; matcher - fn [state node] that decides if node needs to be updated
;;; make-state - fn [state node] that produces new state based on state and node
(defn ast-edit
  [ast editor & [matcher make-state]]
  (let [z (make-ast-zipper ast)]
    (loop [loc z
           stack nil
           depth 0]
      (if (zip/end? loc)
        (zip/root loc)
        (let [node (zip/node loc)
              parent-state (first stack)
              state (when make-state (make-state parent-state node))
              next-node (if-let [mr (when matcher (matcher state node))]
                          (zip/next (zip/edit loc (partial editor mr state)))
                          (zip/next loc))
              next-depth (-> next-node zip/path count)
              next-stack (cond
                          (= next-depth depth) stack ;sibling
                          (> next-depth depth) (cons state stack) ;child
                          :else (nthnext stack next-depth))] ;new branch
          ;; (prn "state" state "pstate" parent-state "NODE:" (zip/node loc) "STACK:" next-stack)
          (recur next-node next-stack next-depth))))))

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
(defn- use-when-editor
  [[use-when state node]]
  node)

(defn- process-use-when
  [ast]
  (ast-edit ast use-when-editor :use-when))

;;; 

;; ;;; AST processing phase: manufacturing of connections between unconnected default readable ports
;; (defmulti insert-default-connections (fn [_ node] (:type node)))
;; (defmethod insert-default-connections :default [_ node]
;;   node)

;;; 

;;; AST processing phase: resolution of imports

(declare parse-import-target)
(defn- imports-editor
  [_ state node]
  (let [content (:content node)
        imports (filter (type-filter :import) content)
        sans-imports (remove (type-filter :import) content)
        steps (reduce (fn [steps import]
                        (let [href (get-attr import qn-a-href)
                              imported-steps (parse-import-target href)]
                          (merge steps imported-steps))) {} imports)] ;TODO duplicates etc.
    (assoc node :content sans-imports :in-scope-types steps)))

(defn- process-imports
  [ast]
  (let [z (make-ast-zipper ast)]
    (util/zipper-edit z imports-editor (h-matcher ::contains-imports))))

;;; AST processing phase: discovering step declarations

(defn- in-scope-step-types
  [loc]
  (loop [parent (zip/up loc)
         types {}]
    (if parent
      (let [node (zip/node parent)
            ntypes (:in-scope-types node)
            merged (merge types ntypes)]
        (recur (zip/up parent) merged))
      types)))

(defn- step-types-editor
  [_ loc]
  (let [node (zip/node loc)
        type (:step-type node)
        in-scope-types (in-scope-step-types loc)]
    (if-let [step (in-scope-types type)]
      node                              ;TODO
      (err-XS0044))))

(defn- resolve-step-types
  [ast]
  (let [z (make-ast-zipper ast)]
    (util/zipper-raw-edit z step-types-editor (h-matcher ::atomic-step))))



;; ;;; AST processing phase: insertion of implicit input/output ports
;; (defmulti insert-implicit-ports (fn [_ node] (:type node)))

;; (defmethod insert-implicit-ports :pipeline [mr node]
;;   (let [content (:content node)
;;         source (create-port port-source :input {qn-a-kind "document" qn-a-sequence "true" qn-a-primary "true"})
;;         parameters (create-port port-parameters :input {qn-a-kind "parameter" qn-a-sequence "true" qn-a-primary "true"})        result (create-port port-result :output {qn-a-sequence "true" qn-a-primary "true"})
                                                         
;;         new-content (vec (concat [source result parameters] content))]
;;     (assoc node :content new-content)))

;; (defmethod insert-implicit-ports :default [_ node]
;;   node)

;;; 


(defn- make-dependency-graph
  [step]
  nil)

;;; 

(defn process-ast
  [ast]
  (let [phases [process-use-when
                process-imports
                resolve-step-types]]
    (reduce (fn [ast phase]
              (phase ast)) ast phases)))

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

;;; 




