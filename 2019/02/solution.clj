(def input (as-> (slurp "input.txt") in
             (clojure.string/trim-newline in)
             (clojure.string/split in #",")
             (map #(Integer/parseInt %) in)
             (into [] in)))

(defn handler [state index func]
  (let [input-index-1 (nth state (+ 1 index))
        input-index-2 (nth state (+ 2 index))
        input-1       (nth state input-index-1)
        input-2       (nth state input-index-2)
        output        (nth state (+ 3 index))]
    (assoc state output (func input-1 input-2))))

(defn intcode-computer [state]
  (loop [state state 
         index 0]
    (let [command (nth state index)
          next-index (+ 4 index)]
      (cond
        (= command 1)  (recur (handler state index +) next-index)
        (= command 2)  (recur (handler state index *) next-index)
        (= command 99) (first state)))))

(println (str "Part 1: " (intcode-computer (-> input
                                               (assoc 1 12)
                                               (assoc 2 2)))))

(def part-2 (first (for [a (range 0 100)
                         b (range 0 100)
                         :let [memory (-> input 
                                          (assoc 1 a) 
                                          (assoc 2 b))
                               output (intcode-computer memory)]
                         :when (= 19690720 output)]
                     (+ (* 100 a) b))))

(println (str "Part 2: " part-2))

