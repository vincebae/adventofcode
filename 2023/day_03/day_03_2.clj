#!/usr/bin/env bb

(ns day-03-2
  (:require [babashka.cli :refer [parse-args]]
            [clojure.string :as s]
            [clojure.test :refer [run-tests deftest is testing]]))

(def ^:private CLI-OPTIONS
  {:alias {:t :test}
   :coerce {:test :boolean}})

(def ^:private GEAR-CHAR \*)

(def ^:private NEIGHBORS
  (for [dy [-1 0 1] dx [-1 0 1] :when (not (and (zero? dy) (zero? dx)))] [dy dx]))

(defn- digit?
  [ch]
  (and (not= ch nil) (Character/isDigit ch)))

(defn- matrixfy
  "Convert collection of strings into a 2D matrix (vector of vector) of characters"
  [schematic]
  {:pre [(not-empty schematic)
         (every? #(= (class %) java.lang.String) schematic)
         (let [col-count (count (first schematic))]
           (every? #(= (count %) col-count) schematic))]}
  (mapv #(mapv identity %) schematic))

(defn- extract-number
  "Find the extract-number that contains the digit in the given position"
  [line pos]
  (let [ch-vec (vec line)]
    (when (digit? (nth ch-vec pos))
      (let [prev (loop [curr-pos (dec pos) acc ()]
                   (let [ch (get ch-vec curr-pos)]
                     (if (digit? ch) (recur (dec curr-pos) (cons ch acc)) (vec acc))))
            next (loop [curr-pos pos acc []]
                   (let [ch (get ch-vec curr-pos)]
                     (if (digit? ch) (recur (inc curr-pos) (conj acc ch)) acc)))
            num-str (apply str (concat prev next))]
        {:num (Integer/parseInt num-str)
         :pos (- pos (count prev))}))))

(defn- neighboring-numbers
  [row col schematic]
  (let [row-count (count schematic)
        col-count (count (first schematic))]
    (->> NEIGHBORS
         (map (fn [[dy dx]] (vector (+ row dy) (+ col dx))))
         (filter (fn [[r c]] (and (<= 0 r (dec row-count)) (<= 0 c (dec col-count)))))
         (filter #(digit? (get-in schematic %)))
         (map (fn [[r c]] (extract-number (get schematic r) c)))
         set
         (mapv :num))))

(defn- all-gear-ratios
  [schematic]
  (let [matrix (matrixfy schematic)
        row-count (count matrix)
        col-count (count (first matrix))
        gears (for [row (range row-count)
                    col (range col-count)
                    :when (= (get-in matrix [row col]) GEAR-CHAR)]
                [row col])]
    (->> gears
         (map (fn [[r c]] (neighboring-numbers r c matrix)))
         (filter #(= (count %) 2))
         (map #(apply * %)))))

(defn- run
  [args opts]
  (->> (slurp (first args))
       (s/split-lines)
       all-gear-ratios
       (apply +)
       (println "answer:")))

(defn- equal-any-order
  [coll1 coll2]
  (= (frequencies coll1) (frequencies coll2)))

(def ^:private TEST-SCHEMATIC ["467..114.."
                               "...*......"
                               "..35..633."
                               "......#..."
                               "617*......"
                               ".....+.58."
                               "..592....."
                               "......755."
                               "...$.*...."
                               ".664.598.."])

;; Test cases
(deftest test-cases
  (testing "test for extract-number"
    (is (= (extract-number "........" 3) nil))
    (is (= (extract-number ".123...." 0) nil))
    (is (= (extract-number ".123...." 1) {:num 123 :pos 1}))
    (is (= (extract-number ".123...." 2) {:num 123 :pos 1}))
    (is (= (extract-number ".123...." 3) {:num 123 :pos 1}))
    (is (= (extract-number ".123...." 4) nil)))
  (testing "test for neighboring-numbers"
    (is (equal-any-order (neighboring-numbers 1 3 TEST-SCHEMATIC)
                         [467 35]))
    (is (equal-any-order (neighboring-numbers 3 6 TEST-SCHEMATIC)
                         [633]))
    (is (equal-any-order (neighboring-numbers 4 3 TEST-SCHEMATIC)
                         [617])))
  (testing "test for all-gear-ratios"
    (is (= (all-gear-ratios TEST-SCHEMATIC) [16345 451490]))))

(defn -main
  [& args]
  (try
    (let [{:keys [args opts] :as arg-map} (parse-args args CLI-OPTIONS)]
      (cond
        (:test opts) (run-tests 'day-03-2)
        :else (run args opts)))
    (catch Exception _
      (println "Invalid input"))))

(apply -main *command-line-args*)
