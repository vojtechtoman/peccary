(ns peccary.testutil
  (:require [peccary.xml :refer :all]
            [peccary.xml.ast :as xmlast]
            [peccary.xml.parse :as xmlparse]))

(defn parse-file
  [file]
  ;; Note: we explicitly realize the whole evts sequence here
  ;; to avoid 'stream closed' errors
  (with-open [f (java.io.FileInputStream. file)]
    (let [evts (xmlparse/parse f)
          c (count evts)]
      evts)))

(defn parse-str
  [s]
  (xmlparse/parse-str s))

(defn evts-ast
  [evts make-ast]
  (make-ast evts))

(defn file-ast
  [file make-ast]
  (-> file parse-file (evts-ast make-ast)))

(defn str-ast
  [s make-ast]
  (-> s parse-str (evts-ast make-ast)))

(defn strip-key
  [ast key]
  (xmlast/ast-edit ast
                   nil
                   [(fn strip-k [state node]
                      {:node (dissoc node key) :state state})]))

(defn strip-ctx
  [ast]
  (strip-key ast :ctx))

(defn ast-eq
  [ast1 ast2]
  (= (strip-ctx ast1) (strip-ctx ast2)))


