(defproject markovist "0.1.0-SNAPSHOT"
  :description "Manifesto seeded Markov chain bot."
  :url "https://www.github.com/jpt4/womb/tree/master/cljexp/markovist"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 ;[clojure-opennlp "0.4.0"] ;uses Opennlp 1.7.2
                 ]
  :main markovist.core
  :aot [markovist.core]
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
