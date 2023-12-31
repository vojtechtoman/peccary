(ns peccary.xproc.compile
  (:gen-class)
  (:require [clojure.java.io :as jio]
            [clojure.zip :as zip]
            [peccary.util :as util]
            [peccary.xml :refer :all]
            [peccary.xml.grammar :as xmlg]
            [peccary.xml.parse :as xmlparse]
            [peccary.xproc.error :refer :all]
            [peccary.xproc.grammar :refer :all]
            [peccary.xproc.vocabulary :refer :all]
            [name.choi.joshua.fnparse :as fp]))


;;; miscellaneous convenience functions to access/manipulate parse tree contents

(defn content
  [node]
  (:content node))

(defn cempty?
  [node]
  (empty? (content node)))

(defn cassoc
  [node c]
  (let [coll (if (sequential? c) c (list c))]
    (assoc node :content coll)))

(defn cset
  [node coll]
  (cassoc node coll))

(defn cprepend
  [node coll]
  (let [content (content node)
        new-content (concat coll content)]
    (cset node new-content)))

(defn cmap
  [pred? f node]
  (let [content (content node)
        new-content (map (fn [n]
                           (if (pred? n)
                             (f n)
                             n))
                         content)]
    (cassoc node new-content)))

(defn cgroup-by
  [f node]
  (let [content (content node)]
    (group-by f content)))

(defn cfilter
  [pred? node]
  (let [content (content node)]
    (filter pred? content)))

(defn cremove
  [pred? node]
  (let [content (content node)]
    (remove pred? content)))

(defn node-type
  [node]
  (:type node))

(defn attrs
  [node]
  (:attrs node))

(defn get-attr
  [node attr]
  (-> node attrs (get attr)))

(defn parse-context
  [node]
  {:pre [(not (nil? (:ctx node)))]}
  (:ctx node))

(defn location
  [node]
  (-> node parse-context :location))

(defn ns-context
  [node]
  (-> node parse-context :ns-context))

(defn ptree->zipper
  [ptree]
  (zip/zipper (fn [n]
                (not (cempty? n)))
              content
              (fn [n c]
                (cassoc n c))
              ptree))

;;; 

;;; A hierarchy of ptree node types that represent various relationships between different node types
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

      (derive ::variable ::with-option)
      (derive ::with-param ::with-option)

))

(defn- hierarchy-type
  ([node]
     (let [type (node-type node)]
       ({:pipeline ::pipeline
         :declare-step ::declare-step
         :library ::library
         :step ::atomic-step
         :with-option ::with-option
         :variable ::variable
         :with-param ::with-param} type)))
  ([state node]                         ;when used as the editor dispatch fn
     (hierarchy-type node)))

(defn- name-attr
  [node]
  (get-attr node qn-a-name))

(defn- hierarchy-type?
  [node type]
  (isa? node-hierarchy (hierarchy-type node) type))

(defn- xproc-posname
  [node]
  (-> node parse-context :posname))

(defn- step-name
  [node]
  (when (hierarchy-type? node ::step)
    (or (get-attr node qn-a-name)
        (xproc-posname node))))

(defn- atomic-step-type-name
  [node]
  (when (hierarchy-type? node ::atomic-step)
    (:step-type node))) 

(defn- attr-as-qname
  [node attr]
  (let [ns-ctx (ns-context node)]
    (-> node (get-attr attr) (as-qname ns-ctx))))


;;; ptree traversal/editing

(defn- e-noop
  "A no-op ptree editor"
  [state node]
  {:state state
   :node node})

;;; various ptree node constructors

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
  ([sname pname]
     {:type :pipe
      :attrs {qn-a-step sname
              qn-a-port pname}})
  ([rp]
     (let [sname (:step rp)
           pname (:port rp)]
       (make-pipe sname pname))))

(defn- make-empty
  []
  {:type :empty})

;;; 

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
  (cfilter port? node))

(defn- document-input-ports
  [node]
  (cfilter document-input-port? node))

(defn- parameter-input-ports
  [node]
  (cfilter parameter-input-port? node))

(defn- output-ports
  [node]
  (cfilter output-port? node))

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
  [sname pname]
  {:step sname :port pname})

(defn- make-readable-port
  [step-node port-selector]
  (when-let [port (port-selector step-node)]
    (let [sname (step-name step-node)
          pname (port-name port)]
      (make-port-ref sname pname))))

;;; 

(defn- binding?
  [node]
  (contains? #{:data :document :empty :pipe} (node-type node)))

(defn- bindings
  [node]
  (cfilter binding? node))

;;; 

(defn- option?
  [node]
  (= :option (node-type node)))

(defn- option-name
  [opt-node]
  (attr-as-qname opt-node qn-a-name))

(defn- options
  [node]
  (cfilter option? node))

(defn- options-map
  [node]
  (let [opts (options node)]
    (reduce (fn [m o]
              (let [oname (option-name o)]
                (assoc m oname o)))
            {}
            opts)))

(defn- required-option?
  "returns nil if not set"
  [opt-node]
  (-> opt-node (get-attr qn-a-required) as-boolean))

(defn- option-default
  [opt-node]
  (get-attr opt-node qn-a-select))

;;; 

(defn- step-declaration?
  [node]
  (hierarchy-type? node ::declare-step))

(defn- make-step-signature
  [content]
  {:content content})

(defn- make-step-type
  [decl]
  (let [ns-ctx (ns-context decl)
        qname (attr-as-qname decl qn-a-type)
        signature (make-step-signature (cfilter (some-fn port? option?)
                                                decl))
        impl decl]
    {:type qname
     :signature signature
     :impl impl}))

(defn- merge-step-type
  [m type]
  (if-let [qname (:type type)]    ;ignore step types with no type name
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
  [state qname]
  (-> state :in-scope-types (get qname)))

;;;
;;; ptree processing phase: filtering based on use-when expressions

(defn- e-use-when
  [state node]
  (e-noop state node))                   ;TODO

;;; 
;;; ptree processing phase: insertion of default input ports

(defn- default-readable-port
  [state]
  (:default-readable-port state))

(defn- set-default-readable-port
  [state drp]
  (assoc state :default-readable-port drp))

(defn- default-readable-parameter-port
  [state]
  (:default-readable-port state))

(defn- set-default-readable-parameter-port
  [state drp]
  (assoc state :default-readable-parameter-port drp))

(defn- step?
  [node]
  (= :step (node-type node)))

(defn- last-step
  [node]
  (reduce (fn [x n]
            (if (step? n)
              n
              x))
          (content node)))

(defn- connect-output-port
  [last-step port]
  (if (primary-port? port)
    (if-let [rp (make-readable-port last-step primary-output-port)]
      (do
        (cassoc port (make-pipe rp)))
      (err-XS0006))
    (cassoc port (make-empty))))

(defn- connect-output-ports
  [node]
  (let [last (last-step node)]
    (cmap output-port? (partial connect-output-port last) node)))

(defn- port-stat-type
  [p]
  (cond (parameter-input-port? p) :i
        (output-port? p) :o
        :else :p))

(defn- port-stats
  [node]
  (let [all-ports (ports node)
        init-stats {:names #{} :i 0 :p 0 :o 0 :primaries #{}}]
    (reduce (fn make-port-ctx [stats p]
              (let [name (port-name p)]
                (if (get-in stats [:names name])
                  ;; port with the same name exists
                  (err-XS0011)
                  (let [type (port-stat-type p)
                        primary (primary-port? p)]
                    (if (and primary
                             (get-in stats [:primaries type]))
                      ;; primary port of the same type already exists
                      (err-XS0030)
                      (-> stats
                          (update-in [:names] #(conj % name))
                          (update-in [type] inc)
                          (update-in [:primaries] #(when primary (conj % type)))))))))
            init-stats all-ports)))

(defn- declare-step-process-ports
  [node]
  ;; 1. For each port category, check that:
  ;;    a) there are no duplicate names
  ;;    b) there is at most one port marked as primary
  ;; 2. For each category:
  ;;    a) if there is only one port (not marked as primary="false"), mark it as primary
  (let [stats (port-stats node)
        new-node (cmap port? (fn [p]
                               (let [type (port-stat-type p)]
                                 (if (or (not= 1 (get stats type))
                                         (false? (primary-port? p)))
                                   ;; not the only port if its type (or explicitly set as primary='false')
                                   p
                                   ;; otherwise mark as primary
                                   (assoc-in p [:attrs qn-a-primary] "true"))))
                       node)]
    new-node))

(defn- declare-step-process-options
  [node]
  ;; err-XS0004 - declaring two options with the same name
  ;; err-XS0017 - declaring an option with both required and default
  (let [options (options node)]
    (do (reduce (fn check-options [s o]
                  (let [name (option-name o)
                        required (required-option? o)
                        default (option-default o)]
                    (if (and required default)
                      (err-XS0017)
                      (if (contains? s name)
                        (err-XS0004)
                        (conj s name))))) #{} options)
        node)))

(defn- pipeline-enter
  [state node]
  (let [node-pp (-> node declare-step-process-ports
                         declare-step-process-options)
        drp (make-readable-port node-pp primary-document-input-port)
        parameter-drp (make-readable-port node-pp primary-parameter-input-port)
        new-state (-> state
                      (set-default-readable-port drp)
                      (set-default-readable-parameter-port parameter-drp))]
    {:state new-state
     :node node-pp}))

(defmulti e-enter hierarchy-type :hierarchy #'node-hierarchy)

(defmethod e-enter ::pipeline
  [state node]
  (let [source (make-port port-source :input
                          {qn-a-kind port-kind-document
                           qn-a-sequence "true"
                           qn-a-primary "true"})
        parameters (make-port port-parameters :input
                              {qn-a-kind port-kind-parameter
                               qn-a-sequence "true"
                               qn-a-primary "true"})
        result (make-port port-result :output {qn-a-sequence "true"
                                               qn-a-primary "true"})
        new-node (cprepend node [source parameters result])]
    (pipeline-enter state new-node)))

(defmethod e-enter ::declare-step
  [state node]
  (pipeline-enter state node))

(defn- add-unspecified-ports
  "Creates a new step node by adding all unspecified ports from signature to step-node"
  [signature node]
  (let [node-ports (ports node)
        node-port-names (into #{} (map port-name node-ports))
        sig-ports (ports signature)
        ;; TODO only add port name, not everything from the declaration
        missing (remove (fn [n]
                          (node-port-names (port-name n)))
                        sig-ports)]
    (cprepend node missing)))

(defn- atomic-step-process-ports
  [node]
  ;; TODO only check for duplicates, do not check the complete port declarations!
  (declare-step-process-ports node))

(defn- option-declared?
  [opt-name signature]
  (let [opts (options signature)]
    (some (fn [n]
            (= opt-name (name-attr n)))
          opts)))

(defn- make-static-with-option
  ([opt-name val & [parse-ctx]]
     {:type :with-option
      :attrs {qn-a-name opt-name
              qn-a-select (str \' val \')} ;TODO use proper XPath escaping/wrapping!
      :content [(make-empty)]
      :ctx parse-ctx}))

(defn- shortcut-options
  [node]
  (let [attrs (attrs node)]
    (into {} (filter (fn [[qname val]]
                       (and (not= qn-a-name qname)
                            (nil? (ns-uri qname))))
                     attrs))))

(defn- merge-shortcut-options
  [node]
  (let [short-opts (shortcut-options node)
        long-opts (options node)
        long-opt-names (into #{} (map (fn [n] (name-attr n))
                                      long-opts))
        parse-ctx (parse-context node)]

    (reduce (fn merge-shortcut-opt [node [opt-name val]]
              (if (contains? long-opt-names opt-name)
                (err-XS0027)    ;specified both in short and long form
                (let [long-form (make-static-with-option opt-name val parse-ctx)]
                  (cprepend node long-form))))
            node short-opts)))

(defn- check-options                    ;note: checks only long-form options
  [signature node]
  ;; - ?? add missing options (taking what is needed from the signature)
  ;; - TODO check for duplicates
  (let [opts (options-map node)
        sopts (options-map signature)
        onames (keys opts)
        sonames (keys sopts)
        unspecified (clojure.set/difference sonames onames)
        unknown (clojure.set/difference onames sonames)]
    (do
      (map (fn check-required [name]
             (when (-> sopts (get name) required-option?)
               (err-XS0018 node name)))  ;required option not specified
           unspecified)
      (when-let [uo (first unknown)]
        (err-XS0031 node uo))             ;unknown option
      node)))

(defn- atomic-step-process-options
  [signature node]
  (->> node
      merge-shortcut-options
      (check-options signature)))

(defn- connect-input-port
  [state port]
  (if (empty? (bindings port))
    (if (primary-port? port)
      (if (parameter-input-port? port)
        (if-let [drp (default-readable-parameter-port state)]
          (cassoc port (make-pipe drp))
          (err-XS0055))                   ;TODO check for p:with-param
        (if-let [drp (default-readable-port state)]
          (cassoc port (make-pipe drp))
          (err-XS0032)))
      (if (parameter-input-port? port)
        (cassoc port (make-empty))
        (err-XS0003)))
    ;; there is a binding already
    port))

(defn- connect-input-ports
  [state node]
  (cmap input-port? (partial connect-input-port state) node))

(defmethod e-enter ::atomic-step
  [state node]
  (let [type-name (atomic-step-type-name node)]
    (if-let [type (get-step-type state type-name)]
      (let [signature (:signature type)
            new-node (->> node
                          (add-unspecified-ports signature)
                          atomic-step-process-ports
                          (atomic-step-process-options signature)
                          (connect-input-ports state))]
        {:state state
         :node new-node})
      (err-XS0044))))

(defmethod e-enter ::with-option
  [state node]
  (if (empty? (bindings node))
    (let [binding (if-let [drp (default-readable-port state)]
                    (make-pipe)
                    (make-empty))]
      (cset node binding))    
    node))

(defmethod e-enter :default
  [state node]
  (e-noop state node))


;;; 
;;; ptree processing phase: insertion of default output ports
(defmulti e-leave hierarchy-type :hierarchy #'node-hierarchy)

(defmethod e-leave ::declare-step
  [state node]
  (let [new-node (connect-output-ports node)
        drp (make-readable-port new-node primary-output-port)
        new-state (set-default-readable-port state drp)]
    {:state new-state
     :node new-node}))

(defmethod e-leave ::atomic-step
  [state node]
  (let [drp (make-readable-port node primary-output-port)
        new-state (set-default-readable-port state drp)]
    {:state new-state
     :node node}))

(defmethod e-leave :default
  [state node]
  (e-noop state node))


;;; 
;;; ptree processing phase: resolution of imports

(defmulti e-imports hierarchy-type :hierarchy #'node-hierarchy)

(declare parse-import-target)
(defmethod e-imports ::contains-imports
  [state node]
  (let [imports-other (cgroup-by #(= :import (node-type %)) node)
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
        new-node (-> node
                     (cassoc other)
                     (assoc :in-scope-types merged-st))]
    {:state new-state
     :node new-node}))

(defmethod e-imports :default
  [state node]
  (e-noop state node))


;;;
;;; ptree processing phase: registering of step names

(defn- add-step-name
  [state node]
  (let [sname (step-name node)]
    (if (get-in state [:in-scope-step-names sname])
      (err-XS0002)
      (update-in state [:in-scope-step-names] #(conj % sname)))))

(defn- add-nested-step-names
  [state node]
  (let [steps (cfilter step? node)]
    (reduce add-step-name state steps)))

(defmulti e-step-names hierarchy-type :hierarchy #'node-hierarchy)

(defmethod e-step-names ::declare-step
  [state node]
  (let [sname (step-name node)
        steps (cfilter step? node)
        new-state (-> state
                      (assoc :in-scope-step-names #{sname})
                      (add-nested-step-names node))]
    {:state new-state
     :node node}))

(defmethod e-step-names ::step
  [state node]
  (let [new-state (add-nested-step-names state node)]
    {:state new-state
     :node node}))

(defmethod e-step-names :default
  [state node]
  (e-noop state node))

;;;
;;; ptree processing phase: discovery of local step declarations

(defmulti e-local-step-declarations hierarchy-type :hierarchy #'node-hierarchy)

(defmethod e-local-step-declarations ::step-source
  [state node]
  (let [decls (cfilter step-declaration? node)
        ist (:in-scope-types state)
        merged-st (merge-step-types ist (map #(make-step-type %) decls))
        new-state (assoc state :in-scope-types merged-st)
        new-node (assoc node :in-scope-types merged-st)]
    {:state new-state
     :node new-node}))

(defmethod e-local-step-declarations :default
  [state node]
  (e-noop state node))


;;; 
;;; ptree processing phase: eliminating dead code

(defn- remove-step-declarations-with-no-type
  [node]
  (let [filtered (cremove (fn [n]
                            (and (step-declaration? n)
                                 (nil? (get-attr n qn-a-type))))
                          node)]
    (cassoc node filtered)))

(defmulti e-eliminate-dead-code hierarchy-type :hierarchy #'node-hierarchy)

(defmethod e-eliminate-dead-code ::step-source
  [state node]
  (let [new-node (-> node remove-step-declarations-with-no-type)]
    {:state state
     :node new-node}))

(defmethod e-eliminate-dead-code :default
  [state node]
  (e-noop state node))


;;; 

(def stdlib (merge-step-types {} [{:type (xproc-qn "identity")
                                   :signature (make-step-signature [(make-port port-source :input
                                                                               {qn-a-kind port-kind-document
                                                                                qn-a-sequence "true"
                                                                                qn-a-primary "true"})
                                                                    (make-port port-result :output
                                                                               {qn-a-sequence "true"
                                                                                qn-a-primary "true"})])
                                   :body 1}]))

;;; 

(defn process-ptree
  [ptree]
  (let [pre-editors [e-use-when            ;process use-when
                     e-eliminate-dead-code ;eliminate dead code
                     e-imports             ;resolve imports
                     e-step-names
                     e-local-step-declarations ;proces local step declarations
                     e-enter]
        post-editors [e-leave]
        initial-state {:in-scope-types stdlib
                       :in-scope-step-names #{}
                       :default-readable-port nil}
        z (ptree->zipper ptree)]
    (util/zip-edit z initial-state pre-editors post-editors)))

(defn evts->pipeline-ptree
  [evts]
  (xmlg/parse main-pipeline-rf evts))

(defn evts->import-target-ptree
  [evts]
  (xmlg/parse import-target-rf evts))

(defn- evts->step-source
  [evts ptree-builder]
  (let [ptree (ptree-builder evts)
        steps (process-ptree ptree)]
    steps))

(defn evts->pipeline-step-source
  [evts]
  (evts->step-source evts evts->pipeline-ptree))

(defn- evts->import-target-step-source
  [evts]
  (evts->step-source evts evts->import-target-ptree))

(defn- parse-import-target
  [x]
  (with-open [s (jio/input-stream x)]
    (let [evts (xmlparse/parse s)]
      (evts->step-source evts))))

