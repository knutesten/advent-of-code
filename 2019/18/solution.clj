(ns solution
  (:require
   [clojure.set :as set]
   [clojure.data.priority-map :refer [priority-map priority-map-keyfn]]
   [clojure.string :as str]))

(defn read-world [file]
  (->> (slurp file)
       str/split-lines
       (mapv vec)))

(defn transform-world [world]
  (let [ceil   (fn [n] (int (Math/ceil n)))
        half-x (ceil (/ (count (first world)) 2))
        half-y (ceil (/ (count world) 2))]
    (-> world
        (assoc-in [(- half-y 2) (- half-x 2)] \@)
        (assoc-in [(- half-y 2) (dec half-x)] \#)
        (assoc-in [(dec half-y) (- half-x 2)] \#)
        (assoc-in [(dec half-y) (dec half-x)] \#)
        (assoc-in [(- half-y 2) half-x] \@)
        (assoc-in [(dec half-y) half-x] \#)
        (assoc-in [half-y (dec half-x)] \#)
        (assoc-in [half-y (- half-x 2)] \@)
        (assoc-in [half-y half-x] \@))))

(defn find-in-world [world pred]
  (for [x     (range (count (first world)))
        y     (range (count world))
        :let  [e (get-in world [y x])]
        :when (pred e)]
    [x y]))

(defn char-is-key? [c]
  (<= 97 (int c) 122))

(defn char-is-door? [c]
  (<= 65 (int c) 90))

(defn find-keys [world]
  (map (juxt #(get-in world (reverse %)) identity)
       (find-in-world world char-is-key?)))

(defn find-doors [world]
  (map (juxt #(get-in world (reverse %)) identity)
       (find-in-world world char-is-door?)))

(defn find-players [world]
  (vec (find-in-world world #(= \@ %))))

(defn find-player [world]
  (first (find-players world)))

(def inf Integer/MAX_VALUE)

(defn dijkstra [start neighbors goal?]
  (loop [open-set  (priority-map start 0)
         g-score   {start 0}
         came-from {}]
    (let [[current] (peek open-set)]
      (cond
        (nil? current)  came-from
        (goal? current) (g-score current)
        :else
        (let [neighs    (map (fn [[n c]] [n (+ c (get g-score current))]) (neighbors current))
              neighs    (filter #(< (second %) (get g-score (first %) inf)) neighs)
              came-from (reduce #(assoc %1 (first %2) current) came-from neighs)
              g-score   (reduce #(assoc %1 (first %2) (second %2)) g-score neighs)
              open-set  (reduce #(assoc %1 (first %2) (second %2)) (pop open-set) neighs)]
          (recur open-set g-score came-from))))))

(defn count-steps-and-doors [world came-from end]
  (let [path  (take-while some? (iterate came-from end))
        dist  (dec (count path))
        doors (->> path
                   (map #(get-in world (reverse %)))
                   (filter char-is-door?)
                   (map #(Character/toLowerCase %))
                   set)]
    (when-not (zero? dist)
      [dist doors])))

(defn create-graph [world player]
  (let [coors (find-keys world)
        neigh #(for [d     [[0 1] [0 -1] [1 0] [-1 0]]
                     :let  [[x y] (map + d %)]
                     :when (not= \# (get-in world [y x]))]
                 [[x y] 1])]
    (into
      {}
      (for [[n coor] (conj coors player)
            :let     [came-from (dijkstra coor
                                          neigh
                                          (constantly false))]]
        [n (->>
             coors
             (remove #(= n (first %)))
             (map (juxt
                    first
                    #(count-steps-and-doors world came-from (second %))))
             (remove #(nil? (second %)))
             (into {}))]))))

(defn neighbors-p1 [graph [ks n]]
  (->> (graph n)
       (filter (fn [[_ [_ drs]]] (set/subset? drs ks)))
       (remove (fn [[k]] (ks k)))
       (map (fn [[v [d _]]]
              [[(conj ks v) v] d]))))

(defn neighbors-p2 [graph [ks ps]]
  (mapcat #(for [[[ks' p'] d] (neighbors-p1 graph [ks %2])]
             [[ks' (assoc ps %1 p')] d])
          (range) ps))

(let [world     (read-world "./input.txt")
      key-count (count (find-keys world))
      goal?     (fn [[ks]] (= key-count (count ks)))

      player (find-player world)
      graph  (create-graph world [\@ player])
      start  [#{} \@]
      part-1 (dijkstra start (partial neighbors-p1 graph) goal?)

      world   (transform-world world)
      players (find-players world)
      graph   (->> (find-players world)
                   (map vector [\1 \2 \3 \4])
                   (map (partial create-graph world))
                   (apply merge))
      start   [#{} [\1 \2 \3 \4]]
      part-2  (dijkstra start (partial neighbors-p2 graph) goal?)]
  (println "Part 1:" part-1)
  (println "Part 2:" part-2))

