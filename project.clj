(defproject peccary "0.1.0-SNAPSHOT"
  :description "An XPork implementation"
  :url "http://bitbucket.org/vojtechtoman/peccary"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [factual/fnparse "2.3.0"]]
  :main ^:skip-aot peccary.core
  :target-path "target/%s"

  :profiles {:uberjar {:aot :all}
             ;; :dev {:dependencies [[midje "1.6.0"]]
             ;;       :plugins [[lein-midje "3.1.3"]]}
})
