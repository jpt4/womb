(defproject tut "0.1.0-SNAPSHOT"
  :description "Introductory Leiningen tutorial project."
  :url "https://github.com/jpt4/womb/tree/master/cljexp/tut"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [clj-http "3.7.0"]]
  :main tut.core
  :aot [tut.core]
  :target-path "target/%s"
  :profiles {:dev {:dependencies [[ring/ring-devel "1.6.3"]]}
             :uberjar {:aot :all}})
