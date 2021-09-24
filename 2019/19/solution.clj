(ns solution
  (:require [intcode.core :refer [parse-file run-program]]))

(defn pull [ins x y]
  (-> (run-program ins :input [y x])
      :output
      first))

(defn calc-width [ins x y]
  (loop [x      x
         offset 0
         len    0
         start  false]
    (let [p (pull ins x y)]
      (cond
        (and start (zero? p)) [offset len]
        (zero? p)             (recur (inc x) (inc offset) len start)
        :else                 (recur (inc x) offset (inc len) true)))))

(defn calc-height [ins x y]
  (loop [y   y
         len 0]
    (let [p (pull ins x y)]
      (if (zero? p)
        len
        (recur (inc y) (inc len))))))

(defn floor [n]
  (int (Math/floor n)))

(defn fit [ins x y]
  (let [[offset x-len] (calc-width ins x y)
        x'             (- (+ offset x x-len) 100)
        y-len          (calc-height ins x' y)]
    [y-len [x' y]]))

(defn find-coor [ins]
  (loop [L 0
         R 1500]
    (let [m         (floor (/ (+ L R) 2))
          [y-len c] (fit ins m m)]
      (cond
        (< R L)       nil
        (< y-len 100) (recur (inc m) R)
        (> y-len 100) (recur L (dec m))
        :else         c))))

(let [ins    (parse-file "./input.txt")
      part-1 (apply +
                    (for [x (range 50)
                          y (range 50)]
                      (pull ins x y)))
      [x y]  (find-coor ins)
      part-2 (+ (* x 10000) y)]
  (println "Part 1:" part-1)
  (println "Part 2:" part-2))

