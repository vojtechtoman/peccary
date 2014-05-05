(ns peccary.io
  (:require [clojure.java.io :refer :all])
  (:import [java.io Reader InputStream
            File FileInputStream
            ByteArrayInputStream CharArrayReader]
           [java.net URI URL MalformedURLException URISyntaxException]))

(defprotocol BaseURIFactory
  "Functions that extract baseURI from varioys input types.

Common options include

:base-uri the base URI to use (overrides the base URI of the argument)

Callers should generally prefer the higher level API provided by
base-uri."
  (make-base-uri [x opts] "Determines the base URI of x."))

(defn- buri
  [uri opts]
  (or (:base-uri opts) uri))

(def default-base-uri-impl
  {:make-base-uri (fn [x opts]
                    (buri nil opts))})

(extend File
  BaseURIFactory
  (assoc default-base-uri-impl
    :make-base-uri (fn [^File x opts]
                     (buri (.getAbsolutePath x) opts))))

(extend URL
  BaseURIFactory
  (assoc default-base-uri-impl
    :make-base-uri (fn [^URL x opts]
                     (buri (.toString x) opts))))

(extend URI
  BaseURIFactory
  (assoc default-base-uri-impl
    :make-base-uri (fn [^URI x opts]
                     (if (.isAbsolute x)
                       (buri (.toString x) opts)
                       (make-base-uri (.toString x) opts)))))

(extend String
  BaseURIFactory
  (assoc default-base-uri-impl
    :make-base-uri (fn [^String x opts]
                     (try
                       (make-base-uri (URL. x) opts)
                       (catch MalformedURLException e
                         (make-base-uri (File. x) opts))))))

(extend Object
  BaseURIFactory
  default-base-uri-impl)

(defn base-uri
  "Attempts to retrive base URI form its argument

Default implementations are defined for File, URI, URL, and String
arguments.

If the argument is a String, it tries to resolve it first as a URI, then
as a local file name. URIs with a 'file' protocol are converted to
local file names."
  [x & [opts]]
  (make-base-uri x (when opts (apply hash-map opts))))

;;; 


(defrecord Source [data base-uri])


(defprotocol SourceFactory
  "Factory functions that create ready-to-use, buffered Sources from
anything that can be unequivocally converted to an
InputStream or Reader.

Common options include

:encoding string name of encoding to use, e.g. 'UTF-8'.
:base-uri the base URI to use (overrides the base URI of the argument)

Callers should generally prefer the higher level API provided by
source."
  (make-source [x opts] "Creates a Source."))


(def default-source-impl
  {:make-source (fn [x opts]
                  (throw (IllegalArgumentException.
                          (str "Cannot open <" (pr-str x) "> as a Source."))))})

(extend InputStream
  SourceFactory
  (assoc default-source-impl
    :make-source (fn [x opts]
                   (let [uri (base-uri x opts)
                         data (input-stream x opts)]
                     (Source. data uri)))))

(extend Reader
  SourceFactory
  (assoc default-source-impl
    :make-source (fn [x opts]
                   (let [uri (base-uri x opts)
                         data (reader x opts)]
                     (Source. data uri)))))

(extend File
  SourceFactory
  (assoc default-source-impl
    :make-source (fn [^File x opts]
                   (make-source (FileInputStream. x) opts))))

(extend URL
  SourceFactory
  (assoc default-source-impl
    :make-source (fn [^URL x opts]
                   (make-source (if (= "file" (.getProtocol x))
                                  (FileInputStream. (.getPath x))
                                  (.openStream x)) opts))))

(extend URI
  SourceFactory
  (assoc default-source-impl
    :make-source (fn [^URI x opts]
                   (make-source (.toURL x) opts))))

(extend String
  SourceFactory
  (assoc default-source-impl
    :make-source (fn [^String x opts]
                   (try
                     (make-source (URL. x) opts)
                     (catch MalformedURLException e
                       (make-source (File. x) opts))))))

(def ^:private byte-array-type (class (make-array Byte/TYPE 0)))

(def ^:private char-array-type (class (make-array Character/TYPE 0)))

(extend byte-array-type
  SourceFactory
  (assoc default-source-impl
    :make-source (fn [x opts]
                   (make-source (ByteArrayInputStream. x) opts))))

(extend char-array-type
  SourceFactory
  (assoc default-source-impl
    :make-source (fn [x opts]
                   (make-source (CharArrayReader. x) opts))))

(extend Object
  SourceFactory
  default-source-impl)

(defn source
  "Attempts to coerce its argument into an open Source.

Default implementations are defined for OutputStream, File, URI, URL,
byte array, char array, and String arguments.

If the argument is a String, it tries to resolve it first as a URI, then
as a local file name. URIs with a 'file' protocol are converted to
local file names.

Should be used inside with-open to ensure the InputStream is properly
closed."
  [x & [opts]]
  (make-source x (when opts (apply hash-map opts))))

