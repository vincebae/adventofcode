#!/usr/bin/env bb

(require '[clojure.string :as s])

(def ^:private CUBES [:red :blue :green])
(def ^:private TOTAL-CUBES {:red 12 :green 13 :blue 14})
(def ^:private LINE-REGEX #"Game ([0-9]+): (.*)")

(defn- parse-set
  [set-str]
  (->> (s/split set-str #",")
       (sequence
        (comp (map s/trim)
              (map #(s/split % #"\s+"))
              (map #(hash-map (keyword (second %)) (Integer/parseInt (first %))))))
       (apply merge)))

(defn- parse-game
  "Parse a line for a single game into a map:
   { :id <id> :blue <# of blues> :red <# of reds> :green <# of greens>}"
  [line]
  (let [[_ id sets-str] (re-matches LINE-REGEX line)]
    (->> (s/split sets-str #";")
         (map parse-set)
         (hash-map :id (Integer/parseInt id) :game))))

(defn- possible-game?
  [game]
  (every? #(<= (get game % 0) (TOTAL-CUBES %)) CUBES))

(defn- read-text
  [args]
  (if (empty? args) (slurp *in*) (slurp (first args))))

(defn -main
  [args]
  (->> (read-text args)
       (s/split-lines)
       (sequence
        (comp (map parse-game)
              (filter #(every? possible-game? (:game %)))
              (map :id)))
       (apply +)
       (println "answer:")))

(-main *command-line-args*)
