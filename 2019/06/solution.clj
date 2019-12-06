(ns adventofcode
  (:require [clojure.set :refer [map-invert difference]]))

(def input (->> (slurp "input.txt")
                (clojure.string/split-lines)
                (map #(clojure.string/split % #"\)"))
                (map #(map keyword %))
                (map reverse)
                (map vec)
                (into {})))

(defn find-orbits [system planet]
  (loop [planet planet
         orbits []]
    (if (= planet :COM)
      orbits
      (let [planet (planet system)
            orbits (conj orbits planet)]
        (recur planet orbits)))))

(defn count-orbits [system]
  (->> (keys system)
       (map (partial find-orbits system))
       (map count)
       (reduce +)))

(println "Part 1:" (count-orbits input))

(defn find-distance [system planet-1 planet-2]
  (let [orbits-1 (into #{} (find-orbits system :YOU))
        orbits-2 (into #{} (find-orbits system :SAN))]
    (->> (difference orbits-1 orbits-2)
         (concat (difference orbits-2 orbits-1))
         (count))))

(println "Part 2:" (find-distance input :YOU :SAN))

