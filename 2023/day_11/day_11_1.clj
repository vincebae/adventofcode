#!/usr/bin/env bb
  
(ns day-11-1
  (:require [babashka.cli :refer [parse-args]]
            [clojure.string :as s]
            [clojure.test :refer [run-tests deftest is testing]]))

(def ^:private cli-options
  {:alias {:t :test}})

(def ^:private EMPTY_SPACE \.)
(def ^:private GALAXY_SPACE \#)

(defn- transpose
  [matrix]
  (apply mapv vector matrix))

(defn- expand-cosmos-vertical
  [cosmos]
  (reduce
   (fn [acc row]
     (if (every? #(= EMPTY_SPACE %) row)
       (-> acc (conj row) (conj row))
       (conj acc row)))
   []
   cosmos))

(defn- expand-cosmos
  [cosmos]
  (->> cosmos
       expand-cosmos-vertical
       transpose
       expand-cosmos-vertical
       transpose
       (mapv #(apply str %))))

(defn- extract-galaxy
  "Extract coordinates of galaxies in the given cosmos as pair.
   Note that the input cosmos is assumed to be already expanded."
  [cosmos]
  (for [row (range (count cosmos))
        col (range (count (first cosmos)))
        :let [ch (get-in cosmos [row col])]
        :when (= ch GALAXY_SPACE)]
    (vector row col)))

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
  [cosmos]
  (->> cosmos
       expand-cosmos
       extract-galaxy
       all-pairs
       (map #(distance (first %) (second %)))
       (apply +)))
        

(defn- run
  [args opts]
  (->> (if (empty? args) (slurp *in*) (slurp (first args)))
       (s/split-lines)
       sum-of-all-distances
       (println "answer:")))

;; Unit test cases

(def ^:private TEST-COSMOS-PRE-EXPANSION
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

(def ^:private TEST-COSMOS-POST-EXPANSION
  ["....#........"
   ".........#..."
   "#............"
   "............."
   "............."
   "........#...."
   ".#..........."
   "............#"
   "............."
   "............."
   ".........#..."
   "#....#......."])

(deftest day-11-1-test-cases
  (testing "Test case for expansion"
    ;; expand vertical only
    (is (= (expand-cosmos ["##" ".."]) ["##" ".." ".."]))
    ;; expand horizontal only
    (is (= (expand-cosmos ["#." "#."]) ["#.." "#.."]))
    ;; no expansion
    (is (= (expand-cosmos ["#." ".#"]) ["#." ".#"]))
    ;; expand both direction
    (is (= (expand-cosmos [".." ".."]) ["...." "...." "...." "...."]))
    ;; more complicated
    (is (= (expand-cosmos TEST-COSMOS-PRE-EXPANSION) TEST-COSMOS-POST-EXPANSION)))
  (testing "Test case for extraction"
    (is (= (extract-galaxy ["##" ".." ".."]) [[0 0] [0 1]]))
    (is (= (extract-galaxy TEST-COSMOS-POST-EXPANSION)
           [[0 4] [1 9] [2 0] [5 8] [6 1] [7 12] [10 9] [11 0] [11 5]])))
  (testing "Test case for distance"
    (is (= (distance [1 3] [1 3]) 0))
    (is (= (distance [0 4] [10 9]) 15))
    (is (= (distance [7 12] [2 0]) 17)))
  (testing "Test case for sum of all distances"
    (is (= (sum-of-all-distances TEST-COSMOS-PRE-EXPANSION) 374))))

;; Main driver

(defn -main
  [& args]
  (try
    (let [{:keys [args opts] :as arg-map} (parse-args args cli-options)]
      (cond
        (:test opts) (run-tests 'day-11-1)
        :else (run args opts)))
    (catch Exception e
      (println "Error:" (.getMessage e)))))

(apply -main *command-line-args*)
