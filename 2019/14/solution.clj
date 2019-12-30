(def reactions
  (->> (slurp "input.txt")
       (clojure.string/split-lines)
       (map #(clojure.string/split % #" => "))
       (map #(concat (clojure.string/split (first %) #", ") (rest %)))
       (map #(map (fn [s] (clojure.string/split s #" ")) %))
       (map #(map (fn [s] (vector (read-string (first s)) (keyword (last s)))) %))
       (map #(map (comp vec reverse) %))
       (map reverse)
       (map
        (fn [[[type amount] & rst]]
          [type
           {:amount amount :cost (into {} rst)}]))
       (into {})))

(defn ceil [n]
  (bigint (Math/ceil n)))

(defn update-values [m f & args]
  (reduce (fn [r [k v]] (assoc r k (apply f v args))) {} m))

(defn calculate-cost [[reaction amount-needed]]
  (when (and (not= :ORE reaction) (pos? amount-needed))
    (let [{amount-provided :amount
           cost            :cost} (reactions reaction)
          factor          (ceil (/ amount-needed amount-provided))]
      (-> cost
          (update-values * factor)
          (assoc reaction (- (* factor amount-provided)))))))

(defn calculate-fuel-cost [fuel]
  (loop [costs {:FUEL fuel}]
    (let [costs    (->> (map calculate-cost costs)
                        (cons costs)
                        (apply merge-with +)
                        (filter #(not= 0 (second %)))
                        (into {}))
          finished (->> costs
                        (filter #(and (not= :ORE (first %)) (pos? (second %))))
                        (empty?))]
      (if finished
        (:ORE costs)
        (recur costs)))))

(println "Part 1:" (calculate-fuel-cost 1))

(defn floor [n]
  (bigint (Math/floor n)))

(defn fuel-compare [fuel goal]
  (let [cost-1 (calculate-fuel-cost fuel)
        cost-2 (calculate-fuel-cost (inc fuel))]
    (cond
      (> cost-1 goal)                        :larger
      (and (<= cost-1 goal) (> cost-2 goal)) :solution
      :else                                  :less)))

(defn fuel-binary-search [goal]
  (loop [l 0
         r (dec goal)]
    (let [m (floor (/ (+ l r) 2))
          c (fuel-compare m goal)]
      (case c
        :solution m
        :less     (recur (inc m) r)
        :larger   (recur l (dec m))))))

(println "Part 2:" (fuel-binary-search 1000000000000))
