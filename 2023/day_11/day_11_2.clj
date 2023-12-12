#!/usr/bin/env bb
  
(ns day-11-2
  (:require [babashka.cli :refer [parse-args]]
            [clojure.string :as s]
            [clojure.test :refer [run-tests deftest is testing]]))

(def ^:private cli-options
  {:alias {:t :test}})

(def ^:private EMPTY-SPACE \.)
(def ^:private GALAXY-SPACE \#)
(def ^:private EXPANSION-FACTOR 1000000)

(defn- extract-galaxy
  "Extract coordinates of galaxies in the given cosmos as pair.
   Note that the input cosmos is assumed to be already expanded."
  [cosmos]
  (for [row (range (count cosmos))
        col (range (count (first cosmos)))
        :let [ch (get-in cosmos [row col])]
        :when (= ch GALAXY-SPACE)]
    (vector row col)))

(defn- expand-row
  [cosmos expansion-factor]
  (loop [[row & rows] (seq cosmos)
         curr 0
         res []]
    (cond
      (nil? row) res
      (every? #(= EMPTY-SPACE %) row) (recur rows (+ curr expansion-factor) (conj res curr))
      :else (recur rows (inc curr) (conj res curr)))))

(defn- expand-col
  [cosmos expansion-factor]
  (loop [[col & cols] (range (count (first cosmos)))
         curr 0
         res []]
    (cond
      (nil? col) res
      (every? #(= EMPTY-SPACE (nth % col)) cosmos) (recur cols (+ curr expansion-factor) (conj res curr))
      :else (recur cols (inc curr) (conj res curr)))))

(defn- expand-coord
  [coord row-expansion col-expansion]
  (vector (get row-expansion (first coord)) (get col-expansion (second coord))))

(defn- all-pairs
  [coll]
  (let [len (count coll)]
    (for [i (range len)
          j (range i len)]
      (vector (nth coll i) (nth coll j)))))

(defn- distance
  [coord1 coord2]
  (->> (map - coord1 coord2)
       (map abs)
       (apply +)))

(defn- sum-of-all-distances
  [cosmos expansion-factor]
  (let [row-expansion (expand-row cosmos expansion-factor)
        col-expansion (expand-col cosmos expansion-factor)]
    (->> cosmos
         extract-galaxy
         (map #(expand-coord % row-expansion col-expansion))
         all-pairs
         (map #(distance (first %) (second %)))
         (apply +))))

(defn- run
  [args opts]
  (->> (if (empty? args) (slurp *in*) (slurp (first args)))
       (s/split-lines)
       (#(sum-of-all-distances % EXPANSION-FACTOR))
       (println "answer:")))

;; Unit test cases

(def ^:private TEST-COSMOS
  ["...#......"
   ".......#.."
   "#........."
   ".........."
   "......#..."
   ".#........"
   ".........#"
   ".........."
   ".......#.."
   "#...#....."])

(deftest day-11-2-test-cases
  (testing "Test case for expand-row"
    (is (= (expand-row [".." ".." "##"] 2) [0 2 4]))
    (is (= (expand-row [".." "##" ".." "##"] 10) [0 10 11 21])))
  (testing "Test case for expand-col"
    (is (= (expand-col ["..#" "..#"] 2) [0 2 4]))
    (is (= (expand-col [".#.#" ".#.#"] 10) [0 10 11 21])))
  (testing "Test case for extraction"
    (is (= (extract-galaxy ["##" ".." ".."]) [[0 0] [0 1]]))
    (is (= (extract-galaxy TEST-COSMOS)
           [[0 3] [1 7] [2 0] [4 6] [5 1] [6 9] [8 7] [9 0] [9 4]])))
  (testing "Test case for expanded-coord"
    (is (= (expand-coord [2 1] [0 2 4] [0 1 2]) [4 1]))
    (is (= (expand-coord [2 2] [0 2 4] [0 1 2]) [4 2])))
  (testing "Test case for distance"
    (is (= (distance [1 3] [1 3]) 0))
    (is (= (distance [0 4] [10 9]) 15))
    (is (= (distance [7 12] [2 0]) 17)))
  (testing "Test case for sum of all distances"
    (is (= (sum-of-all-distances TEST-COSMOS 2) 374))
    (is (= (sum-of-all-distances TEST-COSMOS 10) 1030))))

;; Main driver

(defn -main
  [& args]
  (try
    (let [{:keys [args opts] :as arg-map} (parse-args args cli-options)]
      (cond
        (:test opts) (run-tests 'day-11-2)
        :else (run args opts)))
    (catch Exception e
      (println "Error:" (.getMessage e)))))

(apply -main *command-line-args*)
