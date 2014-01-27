(ns peccary.core
  (:gen-class)
  (:require 
   [peccary.xml.util :as xmlutil]
   [peccary.xml.ast :as xmlast]
   [peccary.xproc.ast :as xprocast]
   [peccary.xproc.vocabulary :refer :all]))



;;; eventually remove this / move to a test module

(def cmt {:type :comment :value "this is a comment"})
(def ws {:type :text :value "        "})

(def se-var {:type :start-element :qname qn-e-variable :attrs {qn-a-name "var" qn-a-select "/*"}})
(def ee-var {:type :end-element :qname qn-e-variable})

(def se-namespaces {:type :start-element :qname qn-e-namespaces})
(def ee-namespaces {:type :end-element :qname qn-e-namespaces})

(def se-empty {:type :start-element :qname qn-e-empty})
(def ee-empty {:type :end-element :qname qn-e-empty})

(def se-declare-step {:type :start-element :qname qn-e-declare-step :attrs {qn-a-type "ex:step"}})
(def ee-declare-step {:type :end-element :qname qn-e-declare-step})

(def evts (list {:type :start-document}           
                se-declare-step
                se-var
                {:type :start-element :qname qn-e-inline :attrs { (xmlutil/qn "ext" "ns") "foo"}}
                {:type :start-element :qname (xmlutil/qn "foo")}
                {:type :end-element :qname (xmlutil/qn "foo")}
                {:type :end-element :qname qn-e-inline}
                se-namespaces
                ee-namespaces
                ee-var
                {:type :start-element :qname (xmlutil/qn "identity" ns-xproc)}
                {:type :end-element :qname (xmlutil/qn "identity" ns-xproc)}
                ee-declare-step
                {:type :end-document}
))

(def evts-step (list
                {:type :start-element :qname (xmlutil/qn "identity" ns-xproc)}
                {:type :end-element :qname (xmlutil/qn "identity" ns-xproc)}))

(def evts-inline
  (list
   {:type :start-element :qname qn-e-inline}
   {:type :start-element :qname (xmlutil/qn "foo")}
   {:type :end-element :qname (xmlutil/qn "foo")}
   {:type :end-element :qname qn-e-inline}))



(defn -main
  [& args]
  (println
   (xmlast/parse xprocast/main-pipeline-g evts)))

