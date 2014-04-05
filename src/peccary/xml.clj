(ns peccary.xml
  (:gen-class)
  (:import [javax.xml XMLConstants]))

(defrecord QName [local-name ns-uri])

;;; (We don't use javax.xml.namespace.QName as its str representations
;;; are not supported by the reader.)
(defn qn
  [local-name & [uri prefix]]
  (let [use-uri (if (empty? uri) nil uri)
        use-prefix (if (empty? prefix) nil prefix)]
    (with-meta (QName. local-name use-uri) {:prefix use-prefix})))

(defn local-name
  [qname]
  (:local-name qname))

(defn ns-uri
  [qname]
  (:ns-uri qname))

(defn prefix
  [qname]
  (-> qname meta :prefix))

;;; some useful constants
(def ^:const ns-xml XMLConstants/XML_NS_URI)
(def ^:const qn-xml-base (qn "base" ns-xml))
(def ^:const qn-xml-id (qn "id" ns-xml))
(def ^:const qn-xml-lang (qn "lang" ns-xml))

(def ^:const ns-xmlns XMLConstants/XMLNS_ATTRIBUTE_NS_URI)
(def ^:const qn-xmlns (qn XMLConstants/XMLNS_ATTRIBUTE ns-xmlns))

;;; 

(defn ns-decl?
  [qname]
  (= ns-xmlns (ns-uri qname)))

;;; TODO throw an error if not an xmlns qname?
(defn ns-prefix
  [qname]
  (when (and (ns-decl? qname)
             (not= qn-xmlns qname))
    (local-name qname)))

