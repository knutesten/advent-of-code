(def input (as-> (slurp "input.txt") in
             (clojure.string/trim-newline in)
             (clojure.string/split in #",")
             (map #(Integer/parseInt %) in)
             (into [] in)))

(defn get-input [memory mode index]
  (if (= mode 0)
    (->> index (nth memory) (nth memory))
    (->> index (nth memory))))

(defn pad-with-zero [modes length]
  (loop [modes  modes]
    (if (= (count modes) length)
      modes
      (recur (conj modes 0)))))

(defn handler-step-4 [modes memory index func]
  (let [modes        (pad-with-zero modes 3)
        input-1      (get-input memory (nth modes 2) (+ 1 index))
        input-2      (get-input memory (nth modes 1) (+ 2 index))
        output-index (get-input memory 1 (+ 3 index))
        output       (func input-1 input-2)
        new-index    (+ 4 index)]
    [(assoc memory output-index output) new-index]))

(defn get-digits [number]
  (as-> number n
    (str n)
    (clojure.string/split n #"")
    (map #(Integer/parseInt %) n)))

(defn parse-instruction [instruction]
  (let [digits (get-digits instruction)
        opcode (->> digits
                    (take-last 2)
                    (apply str)
                    (Integer/parseInt))
        modes (->> digits
                   (butlast)
                   (butlast))]
    [opcode modes]))

(defn jump-handler [modes memory index func]
  (let [modes (pad-with-zero modes 2)
        input-1 (get-input memory (nth modes 1) (+ 1 index))
        input-2 (get-input memory (nth modes 0) (+ 2 index))]
    (if (func input-1)
      [memory input-2]
      [memory (+ 3 index)])))

(defn handle [instruction memory index input]
  (let [[opcode modes] (parse-instruction instruction)]
    (cond
      (= opcode 1) (handler-step-4 modes memory index +)
      (= opcode 2) (handler-step-4 modes memory index *)
      (= opcode 3) [(assoc memory (nth memory (inc index)) input) (+ 2 index)]
      (= opcode 4) (do 
                     (println (get-input memory 0 (inc index)))
                     [memory (+ 2 index)])
      (= opcode 5) (jump-handler modes memory index #(not= 0 %))
      (= opcode 6) (jump-handler modes memory index #(= 0 %))
      (= opcode 7) (handler-step-4 modes memory index #(if (< %1 %2) 1 0))
      (= opcode 8) (handler-step-4 modes memory index #(if (= %1 %2) 1 0))

      (= opcode 99) [nil nil]
      :else (println "ERROR:" opcode)
      )))

(defn run-intcode-computer [memory input]
  (loop [memory memory 
         index 0]
    (let [instruction (nth memory index)
          [new-memory new-index] (handle instruction memory index input)]
      (when-not (nil? new-index)
        (recur new-memory new-index)))))

(println "Part 1:")
(run-intcode-computer input 1)
(println)

(println "Part 2:")
(run-intcode-computer input 5)

