(defn abs [nr]
  (if (neg? nr)
    (* -1 nr)
    nr))

(defn create-cycle [n]
  (rest (cycle (concat (repeat n 0)
                       (repeat n 1)
                       (repeat n 0)
                       (repeat n -1)))))

(defn phase [cycles coll]
  (->> cycles
       (map #(map * coll %))
       (map #(apply + %))
       (map #(rem (abs %) 10))))

(defn phase-part-2 [coll]
  (reduce (fn [acc x] (cons (mod (+ (or (first acc) 0) x) 10) acc))
          '()
          (reverse coll)))

(let [input (slurp "./input.txt")
      skip  (Integer/parseInt (apply str (take 7 input)))
      start (mapv #(Integer/parseInt (str %)) input)

      cycles (take (count start) (map create-cycle (iterate inc 1)))
      part-1 (->> (iterate (partial phase cycles) start)
                  (take 101)
                  last
                  (take 8)
                  (apply str))

      start  (drop skip (take (* 10000 (count input)) (cycle start)))
      part-2 (->> (iterate phase-part-2 start)
                  (take 101)
                  last
                  (take 8)
                  (apply str))]
  (println "Part 1:" part-1)
  (println "Part 2:" part-2))

