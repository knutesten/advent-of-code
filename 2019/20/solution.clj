(ns solution
  (:require
   [clojure.string :as str]
   [clojure.data.priority-map :refer [priority-map]]))

(defn read-world [file]
  (->> (slurp file)
       str/split-lines
       (mapv vec)))

(defn find-portals [world]
  (let [letter? (fn [c] (and c (<= 65 (int c) 90)))]
    (for [x     (range (count (first world)))
          y     (range (count world))
          :let  [c (get-in world [y x])
                 c2 (get-in world [y (inc x)])
                 c3 (get-in world [(inc y) x])]
          :when (and (letter? c)
                     (or (letter? c2)
                         (letter? c3)))]
      (if (letter? c2)
        (if (= \. (get-in world [y (+ 2 x)]))
          [(str c c2) [(+ 2 x) y]]
          [(str c c2) [(dec x) y]])
        (if (= \. (get-in world [(+ 2 y) x]))
          [(str c c3) [x (+ 2 y)]]
          [(str c c3) [x (dec y)]])))))

(defn create-portal-map [world]
  (let [ps (find-portals world)]
    [(second (first (filter #(= "AA" (first %)) ps)))
     (second (first (filter #(= "ZZ" (first %)) ps)))
     (->> ps
          (remove #(#{"AA" "ZZ"} (first %)))
          (group-by first)
          vals
          (map #(map second %))
          (mapcat (fn [[a b]] [[a b] [b a]]))
          (into {}))]))

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

(defn neighbors-p1 [world portals p]
  (concat
    (when (portals p) [[(portals p) 1]])
    (for [dir   [[1 0] [-1 0] [0 1] [0 -1]]
          :let  [[x y] (map + dir p)
                 c (get-in world [y x])]
          :when (= \. c)]
      [[x y] 1])))

(defn inner-outer [world]
  (let [ps      (->> (find-portals world)
                     (remove #(#{"AA" "ZZ"} (first %)))
                     (map second))
        outer-x #{2 (- (count (first world)) 3)}
        outer-y #{2 (- (count world) 3)}
        outer?  (fn [[x y]] (or (outer-x x)
                                (outer-y y)))]
    [(set (remove outer? ps)) (set (filter outer? ps))]))

(defn neighbors-p2 [world portals inner outer [level p]]
  (concat
    (when (or (inner p)
              (and (outer p) (not= level 0)))
      [[[(if (inner p) (inc level) (dec level)) (portals p)] 1]])
    (for [dir   [[1 0] [-1 0] [0 1] [0 -1]]
          :let  [[x y] (map + dir p)
                 c (get-in world [y x])]
          :when (= \. c)]
      [[level [x y]] 1])))

(let [world     (read-world "./input.txt")
      [start
       goal
       portals] (create-portal-map world)
      neigh     (partial neighbors-p1 world portals)
      goal?     #(= goal %)
      part-1    (dijkstra start neigh goal?)

      [inner
       outer] (inner-outer world)
      neigh   (partial neighbors-p2 world portals inner outer)
      goal?   #(= [0 goal] %)
      part-2  (dijkstra [0 start] neigh goal?)
      ]
  (println "Part 1:" part-1)
  (println "Part 2:" part-2))

