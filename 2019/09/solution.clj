(defn digits [number]
  (as-> number n
    (str n)
    (clojure.string/split n #"")
    (map #(bigint %) n)))

(defn opcode [{memory :memory
               index :index }]
  (let [inst (get memory index)]
    (->> (digits inst)
         (take-last 2)
         (apply str)
         (bigint))))

(defn modes [memory index length]
  (let [digits (digits (get memory index))
        modes (->> digits
                   (butlast)
                   (butlast))]
    (loop [modes modes]
      (if (>= (count modes) length)
        modes
        (recur (conj modes 0))))))

(defn param [{relative-base :relative-base memory :memory} mode index]
  (or (cond 
		(= mode 0) (get memory (get memory index))
    (= mode 1) (get memory index 0)
    (= mode 2) (->> index
                    (get memory)
										(+ relative-base) 
                    (get memory))) 0))

(defn params [state index nr]
	(let [memory (:memory state)]
  (->> (range (inc index) (+ 1 index nr))
       (map vector (reverse (modes memory index nr)))
       (map #(param state (first %) (second %))))))

(defn handler [state steps f]
  (let [{memory :memory
         index :index
         relative-base :relative-base} state
        inputs (params state index (dec steps))
        mode   (first (modes memory index 3))
        output (param state 1 (+ steps index))
        output (if (= 2 mode) (+ output relative-base) output)]
    (-> state
        (assoc-in [:memory output] (apply f inputs))
        (assoc :index (+ index (inc steps))))))

(defn jump-handler [state pred]
  (let [{memory :memory
         index :index} state
        [input-1 input-2] (params state index 2)]
    (assoc state :index (if (pred input-1)
                          input-2
                          (+ 3 index)))))

(defmulti intcode-computer opcode)

(defmethod intcode-computer 1 [state] 
  (handler state 3 +)) 

(defmethod intcode-computer 2 [state] 
  (handler state 3 *)) 

(defmethod intcode-computer 3 [state]
  (let [{memory :memory
         index  :index
         input :input
         relative-base :relative-base} state
        mode   (first (modes memory index 1))
        output (param state 1 (inc index))
        output (if (= 2 mode) (+ output relative-base) output)]
    (-> state
        (assoc-in [:memory output] input)
        (assoc :index (+ 2 index)))))


(defmethod intcode-computer 4 [state]
  (let [{memory :memory
         index  :index
         output :output} state
        mode   (first (modes memory index 1))
        result (param state mode (inc index))]
    (-> state
				(assoc :output (conj output result))
        (assoc :last-output result)
        (assoc :index (+ 2 index)))))

(defmethod intcode-computer 5 [state]
  (jump-handler state #(not= 0 %)))

(defmethod intcode-computer 6 [state]
  (jump-handler state #(= 0 %)))

(defmethod intcode-computer 7 [state]
  (handler state 3 #(if (< %1 %2) 1 0)))

(defmethod intcode-computer 8 [state]
  (handler state 3 #(if (= %1 %2) 1 0)))

(defmethod intcode-computer 9 [state]
  (let [{memory :memory
         index  :index
         relative-base :relative-base} state
        mode   (first (modes memory index 1))
        result (param state mode (inc index))]
    (-> state
        (assoc :relative-base (+ relative-base result))
        (assoc :index (+ 2 index)))))

(defmethod intcode-computer 99 [state]
  (assoc state :finished true))

(defn run-intcode-computer [init-memory input]
  (loop [state {:index 0 
                :relative-base 0
                :memory init-memory
								:input input
								:output []}]
    (if (:finished state)
      (first (:output state))
      (recur (intcode-computer state)))))

(def input (as-> (slurp "input.txt") in
             (clojure.string/trim-newline in)
             (clojure.string/split in #",")
             (map #(bigint %) in)
						 (map-indexed #(do [%1 %2]) in)
             (into (sorted-map) in)))

(println "Part 1:" (run-intcode-computer input (bigint 1)))
(println "Part 2:" (run-intcode-computer input (bigint 2)))

