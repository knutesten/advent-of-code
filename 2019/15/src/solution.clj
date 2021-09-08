(ns solution
  (:require
   [intcode.core :refer [run-program parse-file]]))

(def ^:private inf Integer/MAX_VALUE)

(defn dijkstra [start neighbors goal? length]
  (loop [q    #{start}
         dist {start 0}
         prev {}]
    (let [u (first (sort-by dist q))
          q (disj q u)]
      (cond
        (nil? u)  [dist prev]
        (goal? u) (reverse (take-while some? (iterate prev u)))
        :else     (let [ns           (neighbors u)
                        ident-len-fn (juxt identity #(+ (length u %) (dist u)))
                        l            (into {} (map ident-len-fn ns))
                        ns           (filter #(< (l %) (get dist % inf)) ns)]
                    (recur
                      (apply conj q ns)
                      (reduce #(assoc %1 %2 (l %2)) dist ns)
                      (reduce #(assoc %1 %2 u) prev ns)))))))

(defn calc-new-states [state]
  (let [dir {1 [ 0  1]
             2 [ 0 -1]
             3 [-1  0]
             4 [ 1  0]}]
    (for [d     [1 2 3 4]
          :let  [next-state (run-program state :input [d])]
          :when (not= 0 (last (:output next-state)))]
      (-> next-state
          (update :coor #(mapv + (or %1 [0 0]) %2) (get dir d))
          (assoc :d d)))))

(defn create-neighbors-fn [ins]
  (let [state-by-coor (atom {[0 0] ins})]
    (fn [{:keys [coor]}]
      (let [state      (get @state-by-coor coor)
            cs         (calc-new-states state)
            cs-by-coor (into {} (map (juxt :coor identity) cs))]
        (swap! state-by-coor merge cs-by-coor)
        (map #(select-keys % [:coor :output]) cs)))))

(defn goal? [state]
  (= 2 (first (:output state))))

(let [ins      (parse-file "../15/input.txt")
      neigh    (create-neighbors-fn ins)
      steps    (dijkstra {:coor [0 0]} neigh goal? (constantly 1))
      part-1   (dec (count steps))
      [dist _] (dijkstra (last steps) neigh (constantly false) (constantly 1))
      part-2   (apply max (vals dist))]
  (println "Part 1:" part-1)
  (println "Part 2:" part-2))
