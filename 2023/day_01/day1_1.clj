#!/usr/bin/env bb

(require '[clojure.string :as s])

(defn- calibrate-line
  [line]
  (->> (re-seq #"[1-9]" line)
       (#(str (first %) (last %)))
       (Integer/parseInt)))

(defn- calibrate-text
  [text]
  (->> (s/split-lines text) 
       (map calibrate-line)
       (apply +)))

(defn- read-text
  [args]
  (if (empty? args) (slurp *in*) (slurp (first args))))

(defn -main
  [args]
  (->> (read-text args)
       calibrate-text
       (println "answer:")))

(-main *command-line-args*)
