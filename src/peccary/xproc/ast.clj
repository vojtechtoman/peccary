(ns peccary.xproc.ast
  (:gen-class)
  (:require [clojure.java.io :refer :all]
            [clojure.zip :as zip]
            [peccary.util :as util]
            [peccary.xml.ast :as xmlast]
            [peccary.xml.parse :as xmlparse]
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
      (derive ::declare-step ::compound-step)))

;;; miscellaneous helper methods

(defn- node-type
  [node & [_]]
  (:type node))

(defn- hierarchy-type
  [node & [_]]
  (let [type (node-type node)]
    ({:pipeline ::pipeline
      :declare-step ::declare-step
      :library ::library
      :step ::atomic-step} type)))

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

(defn- atomic-step-type-name
  [node]
  (when (hierarchy-type? node ::atomic-step)
    (:step-type node)))                 ;TODO QName!



;;; AST traversal/editing

(defn- make-ast-zipper
  [ast]
  (zip/zipper (fn [n]
                (not (empty? (:content n))))
              :content
              (fn [n c]
                (assoc n :content c))
              ast))


(defn- apply-editors
  [loc state editors]
  (let [node (zip/node loc)
        ectx (reduce (fn [ctx editor]
                       (let [node (:node ctx)
                             state (:state ctx)]
                         (merge {:node node
                                 :state state}
                                (editor node state))))
                     {:node node :state state}
                     editors)
        enode (:node ectx)
        estate (:state ectx)
        eloc (zip/replace loc enode)]
    {:loc eloc
     :state estate}))

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
  {:node node
   :state state})

;;; various AST node constructors

(defn- make-port
  [name type & [{kind qn-a-kind
                 sequence qn-a-sequence
                 primary qn-a-primary}]]
  (let [attrs {qn-a-port name
               qn-a-kind kind
               qn-a-sequence sequence
               qn-a-primary primary}]
    {:type type
     :attrs attrs}))

(defn- make-pipe
  ([step-name port-name]
     {:type :pipe
      :attrs {qn-a-step step-name
              qn-a-port port-name}})
  ([rp]
     (let [step (:step rp)
           port (:port rp)]
       (make-pipe step port))))

(defn- make-empty
  []
  {:type :empty})


;;; port-related



(defn- port-name
  [port-node]
  (get-attr port-node qn-a-port))

(defn- port-kind
  "may return nil"
  [port-node]
  (-> port-node (get-attr qn-a-kind)))

(defn- port?
  [node]
  (contains? #{:input :output} (node-type node)))

(defn- input-port?
  [node]
  (= :input (node-type node)))

(defn- output-port?
  [node]
  (= :output (node-type node)))

(defn- parameter-input-port?
  [node]
  (and (input-port? node)
       (= port-kind-parameter (port-kind node))))

(defn- document-input-port?
  [node]
  (and (input-port? node)
       (not= port-kind-parameter (port-kind node))))

(defn- primary-port?
  "returns nil if not set"
  [port-node]
  (-> port-node (get-attr qn-a-primary) as-boolean))

(defn- ports
  [node]
  (let [content (:content node)]
    (filter port? content)))

(defn- document-input-ports
  [node]
  (let [content (:content node)]
    (filter document-input-port? content)))

(defn- parameter-input-ports
  [node]
  (let [content (:content node)]
    (filter parameter-input-port? content)))

(defn- output-ports
  [node]
  (let [content (:content node)]
    (filter output-port? content)))

(defn- select-primary-port
  "returns nil if there is no primary port among ports"
  [ports]
  ;; candidates: input ports that do not explicitly specify primary="false"
  (when-let [candidates (remove (fn [n]
                                  (false? (primary-port? n)))
                                ports)]
    (if (= 1 (count candidates))
      (first candidates)
      (some (fn [n]
              (when (primary-port? n) n))
            candidates))))

(defn- primary-document-input-port
  [node]
  (let [ports (document-input-ports node)]
    (select-primary-port ports)))

(defn- primary-parameter-input-port
  [node]
  (let [ports (parameter-input-ports node)]
    (select-primary-port ports)))

(defn- primary-output-port
  [node]
  (let [ports (output-ports node)]
    (select-primary-port ports)))

;;; 

(defn- make-port-ref
  [step-name port-name]
  {:step step-name :port port-name})

(defn- make-readable-port
  [step-node port-selector]
  (when-let [port-name (port-selector step-node)]
    (let [step-name (step-name step-node)]
      (make-port-ref step-name port-name))))

;;; 

(defn- binding?
  [node]
  (contains? #{:data :document :empty :pipe} (node-type node)))

(defn- bindings
  [node]
  (let [content (:content node)]
    (filter binding? content)))

;;; 

(defn- step-declaration?
  [node]
  (hierarchy-type? node ::declare-step))

(defn- make-step-signature
  [content]
  {:content content})

(defn- make-step-type
  [decl]
  (let [qname (get-attr decl qn-a-type) ;TODO as QName!
        content (:content decl)
        signature (make-step-signature (filter (fn [n]
                                                 (#{:input :output :option} (node-type n)))
                                               content))
        impl decl]
    {:name qname
     :signature signature
     :impl impl}))

(defn- merge-step-type
  [m type]
  (if-let [qname (:name type)]    ;ignore step types with no type name
    (if (contains? m qname)
      (err-XS0036)
      (assoc m qname type))))

(defn- merge-step-types
  [m types]
  (reduce (fn [m type]
            (merge-step-type m type))
          m
          types))

(defn- get-step-type
  [qname state]
  (-> state :in-scope-types (get qname)))

;;;
;;; AST processing phase: filtering based on use-when expressions

(defn- e-use-when
  [node state]
  (e-noop node state))                   ;TODO

;;; 
;;; AST processing phase: insertion of default input ports

(defn- check-ports
  [node]
  (let [all-ports (ports node)]
    (do
      ;; first detect duplicate port names
      (reduce (fn [m n]
                (let [port-name (port-name n)]
                  (if (contains? m port-name)
                    (err-XS0011)
                    (assoc m port-name n))))
              {}
              all-ports)
      ;; then check that for each port type, there is at most one port declared as primary="true" TODO
      (reduce (fn [s n] (when (primary-port? n)
                          (let [kind (port-kind n)]
                            (if (s kind) ;there already is a primary port of given kind
                              (err-XS0030)
                              (conj s kind)))))
              #{}
              all-ports)
      true)))

(defn- pipeline-check-and-set-internal-readable-ports
  [node state]
  (do (check-ports node)
      (let [drp (make-readable-port node primary-document-input-port)
            parameter-drp (make-readable-port node primary-parameter-input-port)
            new-state (-> state
                          (assoc :default-readable-port drp)
                          (assoc :default-readable-parameter-port parameter-drp))]
        {:node node
         :state new-state})))

(defmulti e-enter-step hierarchy-type)

(defmethod e-enter-step ::pipeline
  [node state]
  (let [content (:content node)
        source (make-port port-source :input
                          {qn-a-kind port-kind-document
                           qn-a-sequence "true"
                           qn-a-primary "true"})
        parameters (make-port port-parameters :input
                              {qn-a-kind port-kind-parameter
                               qn-a-sequence "true"
                               qn-a-primary "true"})
        result (make-port port-result :output {qn-a-sequence "true"
                                               qn-a-primary "true"})
        new-content (concat [source parameters result] content)
        new-node (assoc node :content new-content)]
    (pipeline-check-and-set-internal-readable-ports new-node state)))

(defmethod e-enter-step ::declare-step
  [node state]
  (pipeline-check-and-set-internal-readable-ports node state))

(defn- connect-input-port
  [port state]
  (if (primary-port? port)
    (if (parameter-input-port? port)
      (if-let [drp (:default-readable-parameter-port state)]
        (make-pipe drp)
        (err-XS0055))                   ;TODO check for p:with-param
      (if-let [drp (:default-readable-port state)]
        (make-pipe drp)
        (err-XS0032)))
    (if (parameter-input-port? port)
      (make-empty)
      (err-XS0003))))

(defn- connect-output-port
  [port state]
  (if (primary-port? port)
    (if-let [drp (:default-readable-port state)]
      (make-pipe drp)
      (err-XS0006))
    (make-empty)))

(defn- connect-input-ports              ;TODO
  [node signature state]
  (let [content (:content node)
        new-content (map (fn [n]
                           (if (input-port? n)
                             (connect-input-port n state)
                             n))
                         content)
        new-node (assoc node :content new-content)]
    new-node))

(defmethod e-enter-step ::atomic-step
  [node state]
  (let [type-name (atomic-step-type-name node)]
    (if-let [type (get-step-type type-name state)]
      (let [signature (:signature type)
            new-node (connect-input-ports node signature state)]
        {:node new-node
         :state state})
      (err-XS0044))))

(defmethod e-enter-step :default
  [node state]
  (e-noop node state))


;;; 
;;; AST processing phase: insertion of default output ports
(defmulti e-leave-step hierarchy-type)

(defmethod e-leave-step ::declare-step
  [node state]
  (let [content (:content node)
        new-content (map (fn [n]
                           (if (output-port? n)
                             (connect-output-port n state)
                             n))
                         content)
        new-node (assoc node :content new-content)
        drp (make-readable-port new-node primary-output-port)
        new-state (assoc state :default-readable-port drp)]
    {:node new-node
     :state new-state}))

(defmethod e-leave-step ::atomic-step
  [node state]
  (let [type-name (atomic-step-type-name node)]
    (if-let [type (get-step-type type-name state)]
      (let [signature (:signature type)
            drp (make-readable-port signature primary-output-port) ;TODO signature does not carry step's name!
            new-state (assoc state :default-readable-port drp)]
        {:node node
         :state new-state})
      (err-XS0044))))

(defmethod e-leave-step :default
  [node state]
  (e-noop node state))


;;; 
;;; AST processing phase: resolution of imports

(defmulti e-imports hierarchy-type)

(declare parse-import-target)
(defmethod e-imports ::contains-imports
  [node state]
  (let [content (:content node)
        imports-other (group-by (type-filter :import) content)
        imports (imports-other true)
        other (imports-other false)
        ist (:in-scope-types state)
        merged-st (reduce (fn [st import]
                            (let [href (get-attr import qn-a-href)
                                  imported-steps (parse-import-target href)]
                              (merge-step-types st imported-steps)))
                          ist
                          imports)
        new-state (assoc state :in-scope-types merged-st)
        new-node (assoc node
                   :content other
                   :in-scope-types merged-st)]
    {:node new-node
     :state new-state}))

(defmethod e-imports :default
  [node state]
  (e-noop node state))


;;;
;;; AST processing phase: discovery of local step declarations

(defmulti e-local-step-declarations hierarchy-type)

(defmethod e-local-step-declarations ::contains-step-declarations
  [node state]
  (let [content (:content node)
        decls (filter step-declaration? content)
        ist (:in-scope-types state)
        merged-st (merge-step-types ist (map #(make-step-type %) decls))
        new-state (assoc state :in-scope-types merged-st)
        new-node (assoc node :in-scope-types merged-st)]
    {:node new-node
     :state new-state}))

(defmethod e-local-step-declarations :default
  [node state]
  (e-noop node state))


;;; 
;;; AST processing phase: eliminating dead code

(defn- remove-step-declarations-with-no-type
  [node]
  (let [content (:content node)
        filtered (remove (fn [n]
                           (and (step-declaration? n)
                                (nil? (get-attr n qn-a-type))))
                         content)]
    (assoc node :content filtered)))

(defmulti e-eliminate-dead-code hierarchy-type)

(defmethod e-eliminate-dead-code ::contains-step-declarations
  [node state]
  (let [new-node (-> node remove-step-declarations-with-no-type)]
    {:node new-node
     :state state}))

(defmethod e-eliminate-dead-code :default
  [node state]
  (e-noop node state))


;;; 

(def stdlib (merge-step-types {} [{:name (xproc-qn "identity")
                                   :signature (make-step-signature [(make-port port-source :input
                                                                               {qn-a-kind port-kind-document
                                                                                qn-a-sequence "true"
                                                                                qn-a-primary "true"})
                                                                    (make-port port-result :output
                                                                               {qn-a-sequence "true"
                                                                                qn-a-primary "true"})])
                                   :body 1}]))

;;; 

(defn process-ast
  [ast]
  (let [pre-editors [e-use-when            ;process use-when
                     e-eliminate-dead-code ;eliminate dead code
                     e-imports             ;resolve imports
                     e-local-step-declarations ;proces local step declarations
                     e-enter-step]
        post-editors [e-leave-step]
        initial-state {:in-scope-types stdlib
                       :default-readable-port nil}]
    (ast-edit ast initial-state pre-editors post-editors)))

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

