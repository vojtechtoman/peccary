(ns peccary.xproc.vocabulary
  (:gen-class)
  (:require [peccary.xml.util :as xmlutil]))


(def ^:const ns-xproc "http://www.w3.org/ns/xproc")
(def ^:const xproc-default-prefix "p")

(def ^:const ns-xproc-step "http://www.w3.org/ns/xproc-step")
(def ^:const xproc-step-default-prefix "c")

(def ^:const ns-xproc-error "http://www.w3.org/ns/xproc-error")
(def ^:const xproc-error-default-prefix "err")

(defn xproc-qn
  [local-name & [prefix]]
  (xmlutil/qn local-name ns-xproc prefix))

(defn xproc-step-qn
  [local-name & [prefix]]
  (xmlutil/qn local-name ns-xproc-step prefix))

(defn xproc-error-qn
  [local-name & [prefix]]
  (xmlutil/qn local-name ns-xproc-error prefix))

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

(def ^:const qn-a-binding (xmlutil/qn "binding"))
(def ^:const qn-a-byte-order-mark (xmlutil/qn "qn-a-byte-order-mark"))
(def ^:const qn-a-cdata-section-elements (xmlutil/qn "qn-a-cdata-section-elements"))
(def ^:const qn-a-content-type (xmlutil/qn "content-type"))
(def ^:const qn-a-doctype-public (xmlutil/qn "qn-a-doctype-public"))
(def ^:const qn-a-doctype-system (xmlutil/qn "qn-a-doctype-system"))
(def ^:const qn-a-element (xmlutil/qn "element"))
(def ^:const qn-a-encoding (xmlutil/qn "qn-a-encoding"))
(def ^:const qn-a-escape-uri-attributes (xmlutil/qn "qn-a-escape-uri-attributes"))
(def ^:const qn-a-except-prefixes (xmlutil/qn "except-prefixes"))
(def ^:const qn-a-exclude-inline-prefixes (xmlutil/qn "except-inline-prefixes"))
(def ^:const qn-a-href (xmlutil/qn "href"))
(def ^:const qn-a-include-content-type (xmlutil/qn "qn-a-include-content-type"))
(def ^:const qn-a-indent (xmlutil/qn "qn-a-indent"))
(def ^:const qn-a-kind (xmlutil/qn "kind"))
(def ^:const qn-a-match (xmlutil/qn "match"))
(def ^:const qn-a-media-type (xmlutil/qn "qn-a-media-type"))
(def ^:const qn-a-method (xmlutil/qn "qn-a-method"))
(def ^:const qn-a-name (xmlutil/qn "name"))
(def ^:const qn-a-normalization-form (xmlutil/qn "qn-a-normalization-form"))
(def ^:const qn-a-omit-xml-declaration (xmlutil/qn "qn-a-omit-xml-declaration"))
(def ^:const qn-a-port (xmlutil/qn "port"))
(def ^:const qn-a-primary (xmlutil/qn "primary"))
(def ^:const qn-a-psvi-required (xmlutil/qn "psvi-required"))
(def ^:const qn-a-required (xmlutil/qn "required"))
(def ^:const qn-a-select (xmlutil/qn "select"))
(def ^:const qn-a-sequence (xmlutil/qn "sequence"))
(def ^:const qn-a-standalone (xmlutil/qn "qn-a-standalone"))
(def ^:const qn-a-step (xmlutil/qn "step"))
(def ^:const qn-a-test (xmlutil/qn "test"))
(def ^:const qn-a-type (xmlutil/qn "type"))
(def ^:const qn-a-undeclare-prefixes (xmlutil/qn "qn-a-undeclare-prefixes"))
(def ^:const qn-a-version (xmlutil/qn "version"))
(def ^:const qn-a-wrapper (xmlutil/qn "wrapper"))
(def ^:const qn-a-wrapper-namespace (xmlutil/qn "wrapper-namespace"))
(def ^:const qn-a-wrapper-prefix (xmlutil/qn "wrapper-prefix"))
(def ^:const qn-a-xpath-version (xmlutil/qn "xpath-version"))

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

(defn as-qname                          ;TODO
  [val]
  val)

(defn as-uri                            ;TODO
  [val]
  val)
