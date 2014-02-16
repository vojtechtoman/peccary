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

(defn- node-type
 [node & [_]]
 (:type node))

(defn- node-hierarchy-type
 [node & [_]]
 (let [type (node-type node)]
   ({:pipeline ::pipeline :declare-step ::declare-step :library ::library :step ::atomic-step} type)))


;;; miscellaneous helper methods

(defn- make-ast-zipper
 [ast]
 (let []
   (zip/zipper (fn [n]
                 (not (empty? (:content n)))) :content (fn [n c] (assoc n :content c)) ast)))


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


(defn- apply-editors
 [loc state editors]
 (let [node (zip/node loc)
       ectx (reduce (fn [ctx editor]
                      (let [node (:node ctx)
                            state (:state ctx)]
                        (merge {:node node :state state} (editor node state))))
                    {:node node :state state}
                    editors)
       enode (:node ectx)
       estate (:state ectx)
       eloc (zip/replace loc enode)]
   {:loc eloc :state estate}))

(defn ast-edit
  [ast & [initial-state pre-editors post-editors]]
 (let [z (make-ast-zipper ast)]
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
           (recur next stack :down)))))))

(defn- type-filter
 "Returns a fn that takes a node and returns true if the type of node is equal to type"
 [type]
 (fn [node & [_]]
   (= type (node-type node))))

(defn- hierarchy-filter
  [type]
  (fn [node & [_]]
    (when (isa? node-hierarchy (node-hierarchy-type node) type)
      type)))

(defn- create-port
 [name type & [{kind qn-a-kind sequence qn-a-sequence primary qn-a-primary}]]
 (let [attrs {qn-a-port name qn-a-kind kind qn-a-sequence sequence qn-a-primary primary}]
   {:type type :attrs attrs}))

(defn- get-attr
 [node attr]
 (-> node :attrs attr))

;;; 

;;; AST processing phase: discovering step declarations



;;; 


(defn- make-dependency-graph
 [step]
 nil)

;;; 

(defn make-pipeline-ast
 [evts]
 (xmlast/parse main-pipeline-rf evts))

(defn make-import-target-ast
 [evts]
 (xmlast/parse import-target-rf evts))

(declare process-ast)
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

(defn- e-noop
  [node state]
  {:node node :state state})

;;; AST processing phase: filtering based on use-when expressions
(defn- e-use-when
 [node state]
 (e-noop node state))                   ;TODO

;;; AST processing phase: insertion default input ports
(defmulti e-default-input-ports node-hierarchy-type)

(defmethod e-default-input-ports ::pipeline
  [node state]
  (let [content (:content node)
        source (create-port port-source :input
                            {qn-a-kind "document" qn-a-sequence "true" qn-a-primary "true"})
        parameters (create-port port-parameters :input
                                {qn-a-kind "parameter" qn-a-sequence "true" qn-a-primary "true"})
        new-content (vec (concat [source parameters] content))
        new-node (assoc node :content new-content)]
    {:node new-node :state state}))

(defmethod e-default-input-ports :default
  [node state]
  (e-noop node state))

;;; AST processing phase: insertion default output ports
(defmulti e-default-output-ports node-hierarchy-type)

(defmethod e-default-output-ports ::pipeline
  [node state]
  (let [content (:content node)
        result (create-port port-result :output
                            {qn-a-sequence "true" qn-a-primary "true"})
        new-content (vec (concat [result] content))
        new-node (assoc node :content new-content)]
    {:node new-node :state state}))

(defmethod e-default-output-ports :default
  [node state]
  (e-noop node state))

;;; AST processing phase: resolution of imports

(defmulti e-imports (hierarchy-filter ::contains-imports))

(defmethod e-imports ::contains-imports
 [node state]
 (let [content (:content node)
       imports-rest (group-by (type-filter :import) content)
       imports (imports-rest true)
       rest (imports-rest false)
       steps (reduce (fn [steps import]
                       (let [href (get-attr import qn-a-href)
                             imported-steps (parse-import-target href)]
                         (merge steps imported-steps))) {} imports)
       new-node (assoc node :content rest :in-scope-types steps)] ;TODO duplicates etc.
   {:node new-node :state state}))

(defmethod e-imports :default
  [node state]
  (e-noop node state))

;;; 

(defn process-ast
  [ast]
  (let [pre-editors [e-use-when e-default-input-ports e-imports]
        post-editors [e-default-output-ports]
        initial-state {}]
    (ast-edit ast initial-state pre-editors post-editors)))
