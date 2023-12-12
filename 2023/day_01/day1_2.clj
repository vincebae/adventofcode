#!/usr/bin/env bb

(require '[clojure.string :as s])

(def digits {"one" 1,
             "two" 2,
             "three" 3,
             "four" 4,
             "five" 5,
             "six" 6,
             "seven" 7,
             "eight" 8,
             "nine" 9})

(def digit-regex #"(one|two|three|four|five|six|seven|eight|nine)")

(defn- transform-single
  [digit-str]
  (str (digits digit-str) (subs digit-str 1)))

(defn- transform-line
  [line]
  (if (re-find digit-regex line)
    (recur (s/replace-first line digit-regex #(transform-single (first %))))
    line))

(defn- calibrate-line
  [line]
  (->> (transform-line line)
       (re-seq #"[1-9]")
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
