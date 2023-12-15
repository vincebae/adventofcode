#!/usr/bin/env bb

(ns day-03-1
  (:require [babashka.cli :refer [parse-args]]
            [clojure.string :as s]
            [clojure.test :refer [run-tests deftest is testing]]))

(def ^:private CLI-OPTIONS
  {:alias {:t :test}
   :coerce {:test :boolean}})

(def ^:private EMPTY-SPACE \.)

(defrecord Number [^long num ^long len ^long row ^long col])

(defn- make-number
  [^String num-str ^long row ^long col]
  (->Number (Integer/parseInt num-str) (count num-str) row col))

(defn- digit?
  [ch]
  (and (not= ch nil) (Character/isDigit ch)))

(defn- sym?
  [ch]
  (and (not= ch EMPTY-SPACE) ((complement digit?) ch)))

(defn- matrixfy
  "Convert collection of strings into a 2D matrix (vector of vector) of characters"
  [schematic]
  {:pre [(not-empty schematic)
         (every? #(= (class %) java.lang.String) schematic)
         (let [col-count (count (first schematic))]
           (every? #(= (count %) col-count) schematic))]}
  (mapv #(mapv identity %) schematic))

(defn- next-number
  "Find the next number from character sequences and the starting position."
  [line start-pos row]
  (loop [[ch & chs] (drop start-pos line)
         curr-pos start-pos
         num-pos nil
         num-seq []]
    (cond
      ;; Add the digit to num sequence.
      (digit? ch) (recur chs (inc curr-pos) (or num-pos curr-pos) (conj num-seq ch))
      ;; Skip non-digit chars if num sequence is not yet found.
      (and (some? ch) (nil? num-pos)) (recur chs (inc curr-pos) num-pos num-seq)
      ;; num sequence finished, so return it.
      :else (and num-pos (make-number (apply str num-seq) row num-pos)))))

(defn- all-numbers-in-line
  "Find all numbers with its position from the the line"
  [line row]
  (loop [start-pos 0 res []]
    (if-let [num (next-number line start-pos row)]
      (let [next-start-pos (+ (:col num) (:len num))]
        (recur next-start-pos (conj res num)))
      res)))

(defn- all-numbers-in-matrix
  "Find all numbers with its position from the matrix."
  [matrix]
  (mapcat all-numbers-in-line matrix (range (count matrix))))

(defn- neighbors
  [^Number num matrix]
  (let [row-count (count matrix)
        col-count (count (first matrix))
        row (:row num)
        col-start (:col num)
        col-end (+ col-start (:len num))
        horizontal-range (range (dec col-start) (+ col-end 1))
        top-neighbors (mapv #(vector (dec row) %) horizontal-range)
        side-neighbors (vector [row (dec col-start)] [row col-end])
        bottom-neighbors (mapv #(vector (inc row) %) horizontal-range)]
    (->> (concat top-neighbors side-neighbors bottom-neighbors)
         (filter (fn [[r c]] (and (<= 0 r (dec row-count)) (<= 0 c (dec col-count)))))
         (map #(get-in matrix %)))))

(defn- part-num?
  [^Number num matrix]
  (->> (neighbors num matrix)
       (some sym?)
       boolean))

(defn- all-part-numbers
  [schematic]
  (let [matrix (matrixfy schematic)]
    (->> matrix
         all-numbers-in-matrix
         (filter #(part-num? % matrix))
         (map :num))))

(defn- run
  [args opts]
  (->> (slurp (first args))
       (s/split-lines)
       all-part-numbers
       (apply +)
       (println "answer:")))

;; Test cases
(deftest test-cases
  (testing "test make-number"
    (is (= (->> (make-number "123" 1 2) (into {}))
           {:num 123 :len 3 :row 1 :col 2}))
    (is (= (->> (make-number "012" 1 2) (into {}))
           {:num 12 :len 3 :row 1 :col 2})))
  (testing "test matrixfy"
    (is (= (matrixfy ["...."]) [[\. \. \. \.]]))
    (is (= (matrixfy ["abcd" "1234"]) [[\a \b \c \d] [\1 \2 \3 \4]])))
  (testing "test next-number"
    (is (= (next-number "abcde" 0 100) nil))
    (is (= (next-number "123..456" 0 100) (make-number "123" 100 0)))
    (is (= (next-number "123..456" 1 100) (make-number "23" 100 1)))
    (is (= (next-number "123..456" 3 100) (make-number "456" 100 5))))
  (testing "test all-numbers-in-line"
    (is (= (all-numbers-in-line ".........." 100) []))
    (is (= (all-numbers-in-line "1.23..456." 100)
           [(make-number "1" 100 0),
            (make-number "23" 100 2),
            (make-number "456" 100 6)])))
  (testing "test all-numbers-in-matix"
    (is (= (all-numbers-in-matrix
            [".........."
             "1.23..456."
             "...7.890.."])
           [(make-number "1" 1 0),
            (make-number "23" 1 2),
            (make-number "456" 1 6),
            (make-number "7" 2 3),
            (make-number "890" 2 5)])))
  (testing "test neighbor"
    (is (= (neighbors (make-number "12" 0 1) (matrixfy ["abcd"])) [\a \d]))
    (is (= (neighbors (make-number "123" 0 0)
                      (matrixfy ["abcd"
                                 "1234"
                                 "!@#$"]))
           [\d \1 \2 \3 \4]))
    (is (= (neighbors (make-number "12" 0 2)
                      (matrixfy ["abcd"
                                 "1234"
                                 "!@#$"]))
           [\b \2 \3 \4]))
    (is (= (neighbors (make-number "1" 2 0)
                      (matrixfy ["abcd"
                                 "1234"
                                 "!@#$"]))
           [\1 \2 \@]))
    (is (= (neighbors (make-number "1" 2 3)
                      (matrixfy ["abcd"
                                 "1234"
                                 "!@#$"]))
           [\3 \4 \#]))
    (is (= (neighbors (make-number "12" 1 1)
                      (matrixfy ["abcd"
                                 "1234"
                                 "!@#$"]))
           [\a \b \c \d \1 \4 \! \@ \# \$])))
  (testing "test part-num?"
    (is (= (part-num? (make-number "123" 0 1)
                      (matrixfy [".123...."]))
           false))
    (is (= (part-num? (make-number "123" 0 1)
                      (matrixfy [".123*..."]))
           true))
    (is (= (part-num? (make-number "123" 1 1)
                      (matrixfy ["........"
                                 ".123...."
                                 "........"]))
           false))
    (is (= (part-num? (make-number "123" 1 1)
                      (matrixfy ["!......."
                                 ".123...."
                                 "........"]))
           true))
    (is (= (part-num? (make-number "123" 1 1)
                      (matrixfy ["..@....."
                                 ".123...."
                                 "........"]))
           true))
    (is (= (part-num? (make-number "123" 1 1)
                      (matrixfy ["....#..."
                                 ".123...."
                                 "........"]))
           true))
    (is (= (part-num? (make-number "123" 1 1)
                      (matrixfy ["........"
                                 "$123...."
                                 "........"]))
           true))
    (is (= (part-num? (make-number "123" 1 1)
                      (matrixfy ["........"
                                 ".123%..."
                                 "........"]))
           true))
    (is (= (part-num? (make-number "123" 1 1)
                      (matrixfy ["........"
                                 ".123...."
                                 "^......."]))
           true))
    (is (= (part-num? (make-number "123" 1 1)
                      (matrixfy ["........"
                                 ".123...."
                                 "..&....."]))
           true))
    (is (= (part-num? (make-number "123" 1 1)
                      (matrixfy ["........"
                                 ".123...."
                                 "....(..."]))
           true)))
  (testing "test case for all-part-numbers"
    (is (= (all-part-numbers ["467..114.."
                              "...*......"
                              "..35..633."
                              "......#..."
                              "617*......"
                              ".....+.58."
                              "..592....."
                              "......755."
                              "...$.*...."
                              ".664.598.."])
           [467 35 633 617 592 755 664 598]))))

(defn -main
  [& args]
  (try
    (let [{:keys [args opts] :as arg-map} (parse-args args CLI-OPTIONS)]
      (cond
        (:test opts) (run-tests 'day-03-1)
        :else (run args opts)))
    (catch Exception _
      (println "Invalid input"))))

(apply -main *command-line-args*)
