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

      (derive ::declare-step ::contains-step-declarations) ;TODO merge with step-source???
      (derive ::library ::contains-step-declarations)

      (derive ::atomic-step ::step)
      (derive ::compound-step ::step)
      (derive ::declare-step ::compound-step)
      ))

;;; miscellaneous helper methods

(defn- node-type
  [node & [_]]
  (:type node))

(defn- hierarchy-type
  [node & [_]]
  (let [type (node-type node)]
    ({:pipeline ::pipeline :declare-step ::declare-step :library ::library :step ::atomic-step} type)))

(defn- get-attr
  [node attr]
  (-> node :attrs (get attr)))

(defn- node-name
  [node]
  (get-attr node qn-a-name))

(defn- type-filter
  "Returns a fn that takes a node and returns true if the type of node is equal to type"
  [type]
  (fn [node & [_]]
    (= type (node-type node))))

(defn- hierarchy-type?
  [node type]
  (isa? node-hierarchy (hierarchy-type node) type))

;; (defn- hierarchy-filter
;;   [type]
;;   (fn [node & [_]]
;;     (when (hierarchy-type? node type)
;;       type)))

(defn- step-name
  [node]
  (when-let [type (hierarchy-type node ::step)]
    (or (get-attr node qn-a-name) "STEP TODO") ;TODO
    ))



;;; AST traversal/editing

(defn- make-ast-zipper
  [ast]
  (let []
    (zip/zipper (fn [n]
                  (not (empty? (:content n)))) :content (fn [n c] (assoc n :content c)) ast)))


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

(defn- e-noop
  "A no-op AST editor"
  [node state]
  {:node node :state state})

;;; 

;;; port-related

(defn- create-port
  [name type & [{kind qn-a-kind sequence qn-a-sequence primary qn-a-primary}]]
  (let [attrs {qn-a-port name qn-a-kind kind qn-a-sequence sequence qn-a-primary primary}]
    {:type type :attrs attrs}))

(defn- port?
  [node]
  (contains? #{:input :output} (node-type node)))

(defn- port-name
  [port-node]
  (get-attr port-node qn-a-port))

(defn- port-ref
  [step port-name]
  {:step step :port port-name})

(defn- port-kind
  "may return nil"
  [port-node]
  (-> port-node (get-attr qn-a-kind)))

(defn- port-primary?
  "returns nil if not set"
  [port-node]
  (-> port-node (get-attr qn-a-primary) as-boolean))

(defn- primary-port
  "returns nil if there is no primary port among ports"
  [ports]
  ;; candidates: input ports that do not explicitly specify primary="false"
  (when-let [candidates (filter (fn [n]
                                  (not (false? (port-primary? n)))) ports)]
    (if (= 1 (count candidates))
      (first candidates)
      (some #(when (port-primary? %) %) candidates))))

(defn- primary-input-port
  [node]
  (let [content (:content node)
        ports (filter (fn [n]
                        (and (= (node-type n) :input)
                             (not= (port-kind n) "parameter"))) content)]
    (primary-port ports)))

(defn- primary-parameter-input-port
  [node]
  (let [content (:content node)
        ports (filter (fn [n]
                        (and (= (node-type n) :input)
                             (= (port-kind n) "parameter"))) content)]
    (primary-port ports)))

(defn- primary-output-port
  [node]
  (let [content (:content node)
        ports (filter (type-filter :output) content)]
    (primary-port ports)))

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

(defn- step-declaration?
  [node]
  (hierarchy-type? node ::declare-step))

(defn- make-step-type
  [decl]
  (let [type (get-attr decl qn-a-type)
        content (:content decl)
        signature (filter (fn [n] (#{:input :output :option} (node-type n))) content)
        impl decl]
    {:type type :signature signature :impl impl}))

(defn- add-step-type
  [m type]
  (if-let [type-name (:type type)]      ;ignore step types with no type name
    (if (contains? m type-name)
      (err-XS0036)
      (assoc m type-name type))))

(defn- add-step-types
  [m types]
  (reduce (fn [m type]
            (add-step-type m type)) m types))

;;;

;;; AST processing phase: filtering based on use-when expressions
(defn- e-use-when
  [node state]
  (e-noop node state))                   ;TODO

;;; AST processing phase: insertion of default input ports

(defn- check-ports
  [node]
  (let [content (:content node)
        ports (reduce (fn [m n]
                        (let [port-name (port-name n)]
                          (if (contains? m port-name)
                            (err-XS0011)
                            (assoc m port-name n))))
                      {} (filter port? content))
        primaries (reduce (fn [s [k v]] (when (port-primary? v)
                                          (if (contains? s k)
                                            (err-XS0030))
                                          (conj s k)))
                          #{} ports)]
    nil))

(defn- pipeline-check-and-set-internal-readable-ports
  [node state]
  (do (check-ports node)
      (let [sname (step-name node)
            drp-name (primary-input-port node)
            drp (when drp-name (port-ref sname drp-name))
            parameter-drp-name (primary-parameter-input-port node)
            parameter-drp (when parameter-drp-name (port-ref sname parameter-drp-name))
            new-state (-> state
                          (assoc :default-readable-port drp)
                          (assoc :default-readable-parameter-port parameter-drp))]
        {:node node :state new-state})))

(defmulti e-enter-step hierarchy-type)

(defmethod e-enter-step ::pipeline
  [node state]
  (let [content (:content node)
        source (create-port port-source :input
                            {qn-a-kind "document" qn-a-sequence "true" qn-a-primary "true"})
        parameters (create-port port-parameters :input
                                {qn-a-kind "parameter" qn-a-sequence "true" qn-a-primary "true"})
        result (create-port port-result :output {qn-a-sequence "true" qn-a-primary "true"})
        new-content (concat [source parameters result] content)
        new-node (assoc node :content new-content)]
    (pipeline-check-and-set-internal-readable-ports new-node state)))

(defmethod e-enter-step ::declare-step
  [node state]
  (pipeline-check-and-set-internal-readable-ports node state))

(defmethod e-enter-step ::atomic-step
  [node state]
  ;TODO bind primary (parameter) input ports + check connections
  (e-noop node state))

(defmethod e-enter-step :default
  [node state]
  (e-noop node state))

;;; AST processing phase: insertion of default output ports
(defmulti e-leave-step hierarchy-type)

(defmethod e-leave-step ::declare-step
  [node state]
  ; TODO connect the primary output port to the drp + check connections of all output ports
  (e-noop node state))

(defmethod e-leave-step ::atomic-step
  [node state]
  (let [sname (step-name node)
        ; TODO find decl -> primary output port
        drp-name (primary-output-port node)
        drp (when drp-name (port-ref sname drp-name))
        new-state (assoc state :default-readable-port drp)]
    {:node node :state new-state}))

(defmethod e-leave-step :default
  [node state]
  (e-noop node state))

;;; AST processing phase: resolution of imports

(defmulti e-imports hierarchy-type)

(defmethod e-imports ::contains-imports
  [node state]
  (let [content (:content node)
        imports-other (group-by (type-filter :import) content)
        imports (imports-other true)
        other (imports-other false)
        steps (reduce (fn [steps import]
                        (let [href (get-attr import qn-a-href)
                              imported-steps (parse-import-target href)]
                          (merge steps imported-steps))) {} imports)
        ist (:in-scope-types state)
        merged-ist (add-step-types ist steps)
        new-state (assoc state :in-scope-types merged-ist)
        new-node (assoc node :content other :in-scope-types merged-ist)]
    {:node new-node :state new-state}))

(defmethod e-imports :default
  [node state]
  (e-noop node state))

;;;

(defmulti e-local-step-declarations hierarchy-type)

(defmethod e-local-step-declarations ::contains-step-declarations
  [node state]
  (let [content (:content node)
        decls (filter step-declaration? content)
        ist (:in-scope-types state)
        merged-ist (add-step-types ist (map #(make-step-type %) decls))
        new-state (assoc state :in-scope-types merged-ist)
        new-node (assoc node :in-scope-types merged-ist)]
    {:node new-node :state new-state}))

(defmethod e-local-step-declarations :default
  [node state]
  (e-noop node state))

;;; AST processing phase: eliminating dead code

(defn- remove-step-declarations-with-no-type
  [node]
  (let [content (:content node)
        filtered (remove (fn [n]
                           (and (step-declaration? n)
                                (nil? (get-attr n qn-a-type)))) content)]
    (assoc node :content filtered)))

(defmulti e-eliminate-dead-code hierarchy-type)

(defmethod e-eliminate-dead-code ::contains-step-declarations
  [node state]
  (let [new-node (-> node remove-step-declarations-with-no-type)]
    {:node new-node :state state}))

(defmethod e-eliminate-dead-code :default
  [node state]
  (e-noop node state))

;;; 

(def stdlib (add-step-types {} [{:type (xproc-qn "identity")
                                 :signature [(create-port port-source :input
                                                          {qn-a-kind "document"
                                                           qn-a-sequence "true"
                                                           qn-a-primary "true"})
                                             (create-port port-result :output
                                                          {qn-a-sequence "true"
                                                           qn-a-primary "true"})]
                                 :body 1}]))

;;;

(defn process-ast
  [ast]
  (let [pre-editors [e-use-when            ;process use-when
                     e-eliminate-dead-code ;eliminate dead code
                     e-imports             ;resolve imports
                     e-local-step-declarations ;proces local step declarations
                     e-enter-step
                     ]
        post-editors [e-leave-step
                      ]
        initial-state {:in-scope-types stdlib
                       :default-readable-port nil}]
    (ast-edit ast initial-state pre-editors post-editors)))
