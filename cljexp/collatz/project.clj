(defproject collatz "0.1.0"
  :description "Collatz conjecture analysis."
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [ubergraph "0.8.2"]
                 [incanter "1.9.3"]]
  :repl-options {:init-ns collatz.core})
