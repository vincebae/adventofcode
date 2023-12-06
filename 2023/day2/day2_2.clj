#!/usr/bin/env bb

(require '[clojure.string :as s])

(def ^:private CUBES [:red :blue :green])
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

(defn- max-cube
  [game cube]
  (->> (map #(get % cube 0) game)
       (apply max)))

(defn- power
  [game]
  (->> (map #(max-cube game %) CUBES)
       (apply *)))

(defn- read-text
  [args]
  (if (empty? args) (slurp *in*) (slurp (first args))))

(defn -main
  [args]
  (->> (read-text args)
       (s/split-lines)
       (sequence
        (comp (map parse-game)
              (map :game)
              (map power)))
       (apply +)
       (println "answer:")))

(-main *command-line-args*)
