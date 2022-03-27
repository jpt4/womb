(ns tut.core
  (:gen-class))

(defn -main
  "Tutorial -main."
  [& args]
  (println "Greetings, Metaverse. Args in are:" args))

(defn Afn [k x1 x2 x3 x4 x5]
  (defn Bfn []
    (let [k1 (- k 1)]
      (Afn k1 (Bfn) x1 x2 x3 x4)
      ))
  (if (<= k 0)
    (+ x4 x5)
    (Bfn)))
