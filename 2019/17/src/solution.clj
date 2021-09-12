(ns solution
  (:require
   [intcode.core :refer [run-program parse-file]]
   [clojure.string :as str]))

(defn world-intersections [world]
  (for [y     (range (count world))
        x     (range (count (first world)))
        :when (= [\# \# \# \# \#]
                 (mapv (fn [[x y]] (get (get world y) x))
                       [[x y]
                        [(inc x) y]
                        [(dec x) y]
                        [x (inc y)]
                        [x (dec y)]]))]
    [x y]))

(defn get-2d [world x y]
  (get (get world y) x))

(defn dist [world coor dir]
  (let [d (count
            (take-while (fn [[x y]] (= \# (get-2d world x y)))
                        (iterate #(map + dir %) (map + dir coor))))]
    [d (vec (str d))]))

(defn start-coor [world]
  (first
    (for [x     (range (count (first world)))
          y     (range (count world))
          :let  [block (get-2d world x y)]
          :when (#{\^ \> \v \<} block)]
      [[x y] ({\^ [0 -1] \> [1 0] \v [0 1] \< [-1 0]} block)])))

(defn turn [world [[x y] dir]]
  (let [turns          {[0 1]  [[[1 0] \L] [[-1 0] \R]]
                        [0 -1] [[[1 0] \R] [[-1 0] \L]]
                        [1 0]  [[[0 -1] \L] [[0 1] \R]]
                        [-1 0] [[[0 -1] \R] [[0 1] \L]]}
        [[d1 t1]
         [d2 t2]]      (turns dir)
        block-on-turn? (fn [[dx dy]] (= \# (get-2d world (+ dx x) (+ dy y))))]
    (cond
      (block-on-turn? d1) [d1 t1]
      (block-on-turn? d2) [d2 t2]
      :else               nil)))

(defn find-path [world]
  (loop [pos (start-coor world)
         ins []]
    (let [[dir t] (turn world pos)]
      (if-not dir
        ins
        (let [[d di] (dist world (first pos) dir)
              pos    [(map + (first pos) (map * dir (repeat d))) dir]
              ins    (concat ins [t] di)]
          (recur pos ins))))))

(defn calc-input-str [path]
  (let [path (apply str path)]
    (first
      (for [a     (range 3 11)
            b     (range 3 11)
            c     (range 3 11)
            :let  [s path
                   A (subs s 0 a)
                   s (str/replace s A "")
                   B (subs s 0 b)
                   s (str/replace s B "")
                   C (subs s 0 c)
                   s (str/replace s C "")]
            :when (= "" s)]
        [(-> path
             (str/replace A "A")
             (str/replace B "B")
             (str/replace C "C")) A B C]))))

(defn separate-actions [str-seq]
  (letfn [(nr? [c] ((set (seq "0123456789")) c))]
    (loop [ss  str-seq
           sep []]
      (if (first ss)
        (if (and (nr? (first ss)) (nr? (last sep)))
          (recur (rest ss) (conj sep (first ss)))
          (recur (rest ss) (conj sep \, (first ss))))
        (rest sep)))))

(let [ins       (parse-file "./input.txt")
      world     (->> ins
                     run-program
                     :output
                     (map char)
                     (apply str)
                     str/split-lines
                     (mapv vec))
      part-1    (->>
                  (world-intersections world)
                  (map (partial apply *))
                  (apply +))
      path      (find-path world)
      input-str (calc-input-str path)
      input     (->> (conj input-str "n")
                     (map seq)
                     (map #(separate-actions %))
                     (map #(concat % [\newline]))
                     flatten
                     (map int)
                     reverse
                     vec)
      ins       (assoc ins 0 2)
      part-2    (->> (run-program ins :input input)
                     :output
                     last
                     str)]
  (println "Part 1:" part-1)
  (println "Part 2:" part-2))

