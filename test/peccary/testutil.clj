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

(defn- strip-ctx
  [ast]
  (xmlast/ast-edit ast
                   nil
                   [(fn strip-ctx [state node]
                      {:node (dissoc node :ctx) :state state})]))
(defn ast-eq
  [ast1 ast2]
  (= (strip-ctx ast1) (strip-ctx ast2)))


