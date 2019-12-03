(ns advent-of-code (:require clojure.set))

(def input (->> (slurp "input.txt")
                (clojure.string/split-lines)
                (map #(clojure.string/split % #","))
                (map #(map (fn [s] [(keyword (subs s 0 1)) (Integer/parseInt (subs s 1 (count s)))]) %))))


(def command-directions 
  {
   :R [ 1  0]
   :L [-1  0]
   :U [ 0  1]
   :D [ 0 -1]
   })

(defn get-coordinates-for-command [start path]
  (let [[command length] path
        start (or start [0 0])]
    (for [n (range 1 (inc length))]
      (->> (get command-directions command)
           (map * [n n])
           (map + start)
           (vec)))))

(defn get-coordinates-for-path [path]
  (reduce #(into %1 (get-coordinates-for-command (last %1) %2))
          []
          path))

(defn manhattan-distance [coor]
  (let [[x y] coor]
    (+ (Math/abs x) (Math/abs y))))


(def coordinates-per-path (map get-coordinates-for-path input))
(def intersections (->> coordinates-per-path
                        (map #(into #{} %))
                        (apply clojure.set/intersection)))

(def part-1 (->> intersections
                 (map manhattan-distance)
                 (apply min)))

(println (str "Part 1: " part-1))


(defn signal-delay-for-path [intersection coordinates]
  (loop [sum 1
         coordinates-left coordinates]
    (if (= (first coordinates-left) intersection)
      sum
      (recur (inc sum) (rest coordinates-left)))))

(defn signal-delay-for-intersection [coordinates-per-path intersection] 
  (->> coordinates-per-path
       (map (partial signal-delay-for-path intersection))
       (reduce +)))

(def part-2 (->> intersections
                 (map (partial signal-delay-for-intersection coordinates-per-path))
                 (apply min)))

(println (str "Part 2: " part-2))

