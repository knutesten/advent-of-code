(ns user
  (:require
   [clojure.string :as str]))

(defn parse-moon-line [s]
  (mapv #(Integer/parseInt %) (re-seq #"-?[0-9]+" s)))

(defn parse-input []
  (->>
    (slurp "input.txt")
    str/split-lines
    (map parse-moon-line)))

(def start-state [(parse-input)
                  [[0 0 0]
                   [0 0 0]
                   [0 0 0]
                   [0 0 0]]])

(defn calc-gravity [m1 m2]
  (cond
    (< m1 m2) 1
    (> m1 m2) -1
    :else     0))

(defn next-velocity [velocs poses]
  (map #(mapv + %1 %2)
       velocs
       (for [m poses]
         (->>
           (map #(map calc-gravity %1 %2) (repeat m) poses)
           (apply (partial map +))))))

(defn next-positions [poses velocs]
  (map #(mapv + %1 %2)
       poses
       velocs))

(defn next-step [[poses velocs]]
  (let [next-velocs (doall (next-velocity velocs poses))
        next-poses  (next-positions poses next-velocs)]
    [next-poses next-velocs]))

(defn calc-energy [[poses velocs]]
  (letfn [(sum-abs [a b] (+ (Math/abs a) (Math/abs b)))]
    (apply
      +
      (map *
           (map #(reduce sum-abs %) poses)
           (map #(reduce sum-abs %) velocs)))))

(def pos-seq (iterate next-step start-state))

(println "Part 1:" (calc-energy (nth pos-seq 1000)))

(defn steps-until-repeat [start-state dim-idx]
  (letfn [(dim [s i] (map #(map (fn [m] (nth m i)) %) s))]
    (loop [state start-state
           n     0]
      (let [next-state (next-step state)]
        (if (= (dim start-state dim-idx)
               (dim next-state dim-idx))
          (inc n)
          (recur next-state (inc n)))))))

(defn gcd
  ([a b] (if (zero? b) a (recur b (mod a b))))
  ([a b & xs] (reduce gcd (concat [a b] xs))))

(defn lcm [& xs]
  (let [d (apply gcd xs)]
    (apply * (map #(/ % d) xs))))

(println "Part 2:"
         (apply lcm
                (pmap (partial steps-until-repeat start-state)
                      (range 3))))

(System/exit 0)

