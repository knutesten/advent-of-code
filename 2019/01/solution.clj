(def input (->> (slurp "input.txt")
                (clojure.string/split-lines)
                (map #(Integer/parseInt %))))

(defn calculate-fuel [fuel]
  (->> fuel
       (map #(/ % 3))
       (map int)
       (map #(- % 2))
       (filter #(> % 0))))

(defn sum-list [list] (reduce + list))

(defn calculate-fuel-recursive [fuel]
  (loop [sum 0 
         fuel fuel]
    (let [new-fuel (calculate-fuel fuel)
          new-sum (+ sum (sum-list new-fuel))]
      (if (empty? new-fuel)
        new-sum
        (recur new-sum new-fuel)))))

(def part-1 (sum-list (calculate-fuel input)))
(def part-2 (calculate-fuel-recursive input))

(println (str "Part 1: " part-1))
(println (str "Part 2: " part-2))

