(ns peccary.xproc.vocabulary
  (:gen-class)
  (:require [peccary.xml :refer :all]))


(def ^:const ns-xproc "http://www.w3.org/ns/xproc")
(def ^:const xproc-default-prefix "p")

(def ^:const ns-xproc-step "http://www.w3.org/ns/xproc-step")
(def ^:const xproc-step-default-prefix "c")

(def ^:const ns-xproc-error "http://www.w3.org/ns/xproc-error")
(def ^:const xproc-error-default-prefix "err")

(defn xproc-qn
  [local-name & [prefix]]
  (qn local-name ns-xproc prefix))

(defn xproc-step-qn
  [local-name & [prefix]]
  (qn local-name ns-xproc-step prefix))

(defn xproc-error-qn
  [local-name & [prefix]]
  (qn local-name ns-xproc-error prefix))

(def ^:const qn-e-catch (xproc-qn "catch"))
(def ^:const qn-e-choose (xproc-qn "choose"))
(def ^:const qn-e-data (xproc-qn "data"))
(def ^:const qn-e-declare-step (xproc-qn "declare-step"))
(def ^:const qn-e-document (xproc-qn "document"))
(def ^:const qn-e-documentation (xproc-qn "documentation"))
(def ^:const qn-e-empty (xproc-qn "empty"))
(def ^:const qn-e-for-each (xproc-qn "for-each"))
(def ^:const qn-e-group (xproc-qn "group"))
(def ^:const qn-e-import (xproc-qn "import"))
(def ^:const qn-e-inline (xproc-qn "inline"))
(def ^:const qn-e-input (xproc-qn "input"))
(def ^:const qn-e-iteration-source (xproc-qn "iteration-source"))
(def ^:const qn-e-library (xproc-qn "library"))
(def ^:const qn-e-log (xproc-qn "log"))
(def ^:const qn-e-namespaces (xproc-qn "namespaces"))
(def ^:const qn-e-option (xproc-qn "option"))
(def ^:const qn-e-otherwise (xproc-qn "otherwise"))
(def ^:const qn-e-output (xproc-qn "output"))
(def ^:const qn-e-pipe (xproc-qn "pipe"))
(def ^:const qn-e-pipeinfo (xproc-qn "pipeinfo"))
(def ^:const qn-e-pipeline (xproc-qn "pipeline"))
(def ^:const qn-e-serialization (xproc-qn "serialization"))
(def ^:const qn-e-try (xproc-qn "try"))
(def ^:const qn-e-variable (xproc-qn "variable"))
(def ^:const qn-e-viewport (xproc-qn "viewport"))
(def ^:const qn-e-viewport-source (xproc-qn "viewport-source"))
(def ^:const qn-e-when (xproc-qn "when"))
(def ^:const qn-e-with-option (xproc-qn "with-option"))
(def ^:const qn-e-with-param (xproc-qn "with-param"))
(def ^:const qn-e-xpath-context (xproc-qn "xpath-context"))

(def ^:const qn-a-binding (qn "binding"))
(def ^:const qn-a-byte-order-mark (qn "qn-a-byte-order-mark"))
(def ^:const qn-a-cdata-section-elements (qn "qn-a-cdata-section-elements"))
(def ^:const qn-a-content-type (qn "content-type"))
(def ^:const qn-a-doctype-public (qn "qn-a-doctype-public"))
(def ^:const qn-a-doctype-system (qn "qn-a-doctype-system"))
(def ^:const qn-a-element (qn "element"))
(def ^:const qn-a-encoding (qn "qn-a-encoding"))
(def ^:const qn-a-escape-uri-attributes (qn "qn-a-escape-uri-attributes"))
(def ^:const qn-a-except-prefixes (qn "except-prefixes"))
(def ^:const qn-a-exclude-inline-prefixes (qn "except-inline-prefixes"))
(def ^:const qn-a-href (qn "href"))
(def ^:const qn-a-include-content-type (qn "qn-a-include-content-type"))
(def ^:const qn-a-indent (qn "qn-a-indent"))
(def ^:const qn-a-kind (qn "kind"))
(def ^:const qn-a-match (qn "match"))
(def ^:const qn-a-media-type (qn "qn-a-media-type"))
(def ^:const qn-a-method (qn "qn-a-method"))
(def ^:const qn-a-name (qn "name"))
(def ^:const qn-a-normalization-form (qn "qn-a-normalization-form"))
(def ^:const qn-a-omit-xml-declaration (qn "qn-a-omit-xml-declaration"))
(def ^:const qn-a-port (qn "port"))
(def ^:const qn-a-primary (qn "primary"))
(def ^:const qn-a-psvi-required (qn "psvi-required"))
(def ^:const qn-a-required (qn "required"))
(def ^:const qn-a-select (qn "select"))
(def ^:const qn-a-sequence (qn "sequence"))
(def ^:const qn-a-standalone (qn "qn-a-standalone"))
(def ^:const qn-a-step (qn "step"))
(def ^:const qn-a-test (qn "test"))
(def ^:const qn-a-type (qn "type"))
(def ^:const qn-a-undeclare-prefixes (qn "qn-a-undeclare-prefixes"))
(def ^:const qn-a-version (qn "version"))
(def ^:const qn-a-wrapper (qn "wrapper"))
(def ^:const qn-a-wrapper-namespace (qn "wrapper-namespace"))
(def ^:const qn-a-wrapper-prefix (qn "wrapper-prefix"))
(def ^:const qn-a-xpath-version (qn "xpath-version"))

;;; 

(def ^:const port-source "source")
(def ^:const port-result "result")
(def ^:const port-parameters "parameters")

(def ^:const port-kind-document "document")
(def ^:const port-kind-parameter "parameter")

;;; 

(defn as-boolean                        ;TODO fail if not boolean?
  [val]
  (let [b ({"true" true true true "1" true 1 true 
            "false" false false false "0" false 0 false} val)]
    b))

(defn as-string
  [val]
  val)

(defn as-ncname                         ;TODO
  [val]
  val)

(defn as-ncnames                        ;TODO
  [val]
  val)

(defn as-nmtoken                        ;TODO
  [val]
  val)

(defn as-nmtokens                       ;TODO
  [val]
  val)

(defn- parse-qname                      ;TODO this is way too simplistic
  [val]
  (if (empty? val)
    (throw (IllegalArgumentException.))                             ;TODO parse error
    (let [parts (clojure.string/split val #":" 2)]
      (if (= 1 (count parts))
        {:local val}
        {:prefix (first parts)
         :local (second parts)}))))

(defn as-qname                          ;TODO
  "returns nil if val is nil"
  [val ns-context]
  (when val
    (let [parsed (parse-qname val)
          prefix (:prefix parsed)
          local (:local parsed)
          ns (when prefix
               (or (get ns-context prefix)
                   (throw (IllegalArgumentException.))))]
        (qn local ns prefix))))

(defn as-uri                            ;TODO
  [val]
  val)
