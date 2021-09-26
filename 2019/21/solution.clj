(ns solution
  (:require
   [intcode.core :refer [run-program parse-file]]))

(defn run [ins commands]
  (last
    (:output
     (run-program ins :input (->> commands
                                  (map #(mapv int %))
                                  (mapcat #(conj % 10))
                                  reverse
                                  vec)))))

(let [ins    (parse-file "./input.txt")
      part-1 (run ins ["NOT C T"
                       "AND D T"
                       "NOT A J"
                       "OR T J"
                       "WALK"])
      part-2 (run ins ["NOT A J"
                       "OR B T"
                       "OR E T"
                       "NOT T T"
                       "OR T J"
                       "OR B T"
                       "OR I T"
                       "NOT T T"
                       "OR T J"
                       "NOT C T"
                       "AND D T"
                       "AND H T"
                       "OR T J"
                       "AND D J"
                       "RUN"])]
  (println "Part 1:" part-1)
  (println "Part 2:" part-2))

