(ns markovist.core
  (:require [clojure.string :as str])
  (:gen-class))

(defn tokenize "Text file -> string of words with no newlines."
  [file & files]
  (str/join #" " (filter (fn [a] (not (str/blank? a))) (map str/trim (str/split-lines (slurp file))))))



(defn fib "nth Fibonacci"
  [num]
  (cond
    (< num 0) 0
    (or (= num 0) (= num 1)) 1
    :else (+ (fib (- num 1)) (fib (- num 2)))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (fib (first args)))


