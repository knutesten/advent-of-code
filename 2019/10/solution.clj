(def input
  (as-> "input.txt" in
        (slurp in)
        (clojure.string/split-lines in)
        (mapv #(clojure.string/split % #"") in)))

(defn in-sight? [p1 p2 p3]
  (let [[dx dy] (map - p2 p1)
        [x' y'] (map - p3 p1)]
    (cond
      (= 0 dx dy) nil
      (= 0 dx)    (and (= 0 x') (< 0 (/ y' dy)))
      (= 0 dy)    (and (= 0 y') (< 0 (/ x' dx)))
      :else       (let [[t1 t2] (map / [x' y'] [dx dy])]
                    (and (= t1 t2) (< 0 t1))))))

(defn vec-len [v]
  (->> v
       (map #(Math/pow % 2))
       (reduce +)
       (Math/sqrt)))

(defn distance [p1 p2]
  (vec-len (map - p2 p1)))

(defn is-asteroid-hidden? [asteroids a1 a2]
    (loop [as (rest asteroids)
           a         (first asteroids)]
      (cond
        (nil? a)            false
        (in-sight? a1 a2 a) true
        :else               (recur (rest as) (first as)))))

(defn asteroids-in-sight [as a]
  (let [as (sort-by #(distance a %) as)]
    (loop [asteroids (filter #(not= a %) as)
           in-sight  []]
      (if (empty? asteroids)
        in-sight
        (let [new-asteroid (first asteroids)
              in-sight     (if (is-asteroid-hidden? in-sight a new-asteroid)
                             in-sight
                             (conj in-sight new-asteroid))]
          (recur (rest asteroids) in-sight))))))

(defn count-asteroids-in-sight [as a]
  (count (asteroids-in-sight as a)))

(defn asteroids [asteroid-map]
  (for [x     (range 0 (count (first asteroid-map)))
        y     (range 0 (count asteroid-map))
        :when (= "#" (-> asteroid-map (nth y) (nth x)))]
    [x y]))

(defn find-max-asteroids-seen [asteroid-map]
  (let [as (asteroids asteroid-map)]
    (->> as
         (pmap #(vector % (count-asteroids-in-sight as %)))
         (reduce #(if (> (second %1) (second %2)) %1 %2)))))

(defn angle [p1 p2]
  (let [v1 [0 -1]
        v2 (map - p2 p1)
        a (Math/acos (/
                      (reduce + (map * v1 v2))
                      (* (vec-len v1) (vec-len v2))))
        [x y] v2]
    (if (neg? x)
      (- (* 2 Math/PI) a)
      a)))

(def result (find-max-asteroids-seen input))
(def best-pos (first result))
(def nr-of-asteroids (second result))
(def _200th (->> best-pos
                 (asteroids-in-sight (asteroids input))
                 (sort-by #(angle best-pos %))
                 (take 200)
                 (last)))

(println "Part 1:" nr-of-asteroids)
(println "Part 2:" (+
                    (* 100 (first _200th))
                    (second _200th)))

(System/exit 0)
