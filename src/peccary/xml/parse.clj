(ns peccary.xml.parse
  (:gen-class)
  (:require [peccary.xml.util :as xmlutil])
  (:import (javax.xml.stream XMLInputFactory
                             XMLStreamReader
                             XMLStreamConstants)))


(defn- start-document
  []
  {:type :start-document})

(defn- end-document
  []
  {:type :end-document})

(defn- start-element
  [local-name ns-uri prefix attrs]
  (let [qname (xmlutil/qn local-name ns-uri prefix)]
    {:type :start-element :qname qname :attrs attrs}))

(defn- end-element
  [local-name ns-uri prefix]
  (let [qname (xmlutil/qn local-name ns-uri prefix)]
    {:type :end-element :qname qname}))

(defn- text
  [data]
  {:type :text :data data})

(defn- comm
  [data]
  {:type :comment :data data})

(defn- cdata
  [data]
  {:type :cdata :data data})

(defn- pi
  [target data]
  {:type :pi :target target :data data})

;;; TODO include also namespace declarations!!!
(defn- attr-hash
  [^XMLStreamReader sreader]
  (into {}
        (for [i (range (.getAttributeCount sreader))]
          (let [local-name (.getAttributeLocalName sreader i)
                ns-uri (.getAttributeNamespace sreader i)
                prefix (.getAttributePrefix sreader i)
                qname (xmlutil/qn local-name ns-uri prefix)
                value (.getAttributeValue sreader i)]
            [qname value]))))

; Note, sreader is mutable and mutated here in pull-seq, but it's
; protected by a lazy-seq so it's thread-safe.
(defn- pull-seq-doc
  "Creates a seq of events. The XMLStreamConstants/SPACE clause below doesn't seem to
be triggered by the JDK StAX parser, but is by others. Leaving in to be more complete."
  [^XMLStreamReader sreader]
  (lazy-seq
   (loop []
     (condp == (.next sreader)
       XMLStreamConstants/START_ELEMENT
       (cons (let [local-name (.getLocalName sreader)
                   ns-uri (.getNamespaceURI sreader)
                   prefix (.getPrefix sreader)
                   attrs (attr-hash sreader)]
               (start-element local-name ns-uri prefix attrs))
             (pull-seq-doc sreader))
       XMLStreamConstants/END_ELEMENT
       (cons (let [local-name (.getLocalName sreader)
                   ns-uri (.getNamespaceURI sreader)
                   prefix (.getPrefix sreader)]
               (end-element local-name ns-uri prefix))
             (pull-seq-doc sreader))
       XMLStreamConstants/CHARACTERS
       (cons (text (.getText sreader))
             (pull-seq-doc sreader))
       XMLStreamConstants/COMMENT
       (cons (comm (.getText sreader))
             (pull-seq-doc sreader))
       XMLStreamConstants/CDATA
       (cons (cdata (.getText sreader))
             (pull-seq-doc sreader))
       XMLStreamConstants/PROCESSING_INSTRUCTION
       (cons (pi (.getPITarget sreader) (.getPIData sreader))
             (pull-seq-doc sreader))
       XMLStreamConstants/END_DOCUMENT
       (list (end-document))
       (recur)
       ))))

(defn- pull-seq
  "Creates a seq of events. The XMLStreamConstants/SPACE clause below doesn't seem to
be triggered by the JDK StAX parser, but is by others. Leaving in to be more complete."
  [^XMLStreamReader sreader]
  (cons
     (start-document)
     (pull-seq-doc sreader)))

(def ^{:private true} xml-input-factory-props
  {:allocator javax.xml.stream.XMLInputFactory/ALLOCATOR
   :coalescing javax.xml.stream.XMLInputFactory/IS_COALESCING
   :namespace-aware javax.xml.stream.XMLInputFactory/IS_NAMESPACE_AWARE
   :replacing-entity-references javax.xml.stream.XMLInputFactory/IS_REPLACING_ENTITY_REFERENCES
   :supporting-external-entities javax.xml.stream.XMLInputFactory/IS_SUPPORTING_EXTERNAL_ENTITIES
   :validating javax.xml.stream.XMLInputFactory/IS_VALIDATING
   :reporter javax.xml.stream.XMLInputFactory/REPORTER
   :resolver javax.xml.stream.XMLInputFactory/RESOLVER
   :support-dtd javax.xml.stream.XMLInputFactory/SUPPORT_DTD})

(defn new-xml-input-factory [props]
  (let [fac (javax.xml.stream.XMLInputFactory/newInstance)]
    (doseq [[k v] props
            :let [prop (xml-input-factory-props k)]]
      (.setProperty fac prop v))
    fac))

(defn- source-seq
  "Parses the XML InputSource source using a pull-parser. Returns
a lazy sequence of ParseEvent records. Accepts key pairs
with XMLInputFactory options, see http://docs.oracle.com/javase/6/docs/api/javax/xml/stream/XMLInputFactory.html
and xml-input-factory-props for more information. Defaults coalescing true."
  [s & {:as props}]
  (let [fac (new-xml-input-factory (merge {:coalescing true} props))
        ;; Reflection on following line cannot be eliminated via a
        ;; type hint, because s is advertised by fn parse to be an
        ;; InputStream or Reader, and there are different
        ;; createXMLStreamReader signatures for each of those types.
        sreader (.createXMLStreamReader fac s)]
    (pull-seq sreader)))

(defn parse
  "Parses the source, which can be an
InputStream or Reader, and returns a lazy tree of Element records. Accepts key pairs
with XMLInputFactory options, see http://docs.oracle.com/javase/6/docs/api/javax/xml/stream/XMLInputFactory.html
and xml-input-factory-props for more information. Defaults coalescing true."
  [source & props]
  (apply source-seq source props))

(defn parse-str
  "Parses the passed in string to Clojure data structures. Accepts key pairs
with XMLInputFactory options, see http://docs.oracle.com/javase/6/docs/api/javax/xml/stream/XMLInputFactory.html
and xml-input-factory-props for more information. Defaults coalescing true."
  [s & props]
  (let [sr (java.io.StringReader. s)]
    (apply parse sr props)))


