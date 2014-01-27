(ns peccary.xml.util
  (:gen-class)
  (:import [javax.xml XMLConstants]))

(defrecord QName [local-name ns-uri prefix])

;;; TODO: At the moment the prefix is significant in the = comparison
;;; which is not correct. We will need a custom type that has map-like
;;; semantics, but which does not use prefix in the = comparison.
;;; (We don't use javax.xml.namespace.QName as its str representations
;;; are not supported by the reader.)
(defn qn
  [local-name & [uri prefix]]
  (let [use-uri (if (empty? uri) nil uri)
        use-prefix (if (empty? prefix) nil prefix)]
    (QName. local-name use-uri use-prefix)))

(defn local-name
  [qname]
  (:local-name qname))

(defn ns-uri
  [qname]
  (:ns-uri qname))

(defn prefix
  [qname]
  (:prefix qname))

(defn qn-eq
  [qn1 qn2]
  (and (= (local-name qn1) (local-name qn2))
       (= (ns-uri qn1) (ns-uri qn2))))

;;; some useful constants
(def ^:const ns-xml XMLConstants/XML_NS_URI)
(def ^:const qn-xml-base (qn "base" ns-xml))

(def ^:const ns-xmlns XMLConstants/XMLNS_ATTRIBUTE_NS_URI)
(def ^:const qn-xmlns (qn XMLConstants/XMLNS_ATTRIBUTE ns-xmlns))

;;; 

(defn namespace-declaration?
  [qname]
  (= ns-xmlns (ns-uri qname)))

;;; TODO throw an error if not an xmlns qname?
(defn namespace-prefix
  [qname]
  (when (and (namespace-declaration? qname) (not= qn-xmlns qname))
    (local-name qname)))

;;; 

(defn resolve-uri
  [parent relative]
  (cond
   (nil? parent) relative
   (nil? relative) parent
   :else relative))                     ;TODO!
