#!/usr/bin/env bb

(require '[clojure.string :as s])

(def test-input
  (s/join
    "\n"
    ["1abc2"
     "pqr3stu8vwx"
     "a1b2c3d4e5f"
     "treb7uchet"]))
 
(defn- calibrate-line
  [line]
  (->> (seq line)
       (filter #(Character/isDigit %))
       (#(str (first %) (last %)))
       (Integer/parseInt)))

(defn- calibrate-text
  [text]
  (->> (s/split text #"\n")
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
