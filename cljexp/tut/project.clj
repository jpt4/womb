(defproject tut "0.1.0-SNAPSHOT"
  :description "Introductory tutorial project."
  :url "https://github.com/jpt4/womb"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]]
  :main ^:skip-aot tut.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
