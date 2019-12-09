(ns user 
  (:require [clojure.core.async :refer [chan go go-loop <! <!! >! >!!]]))

(defn digits [number]
  (as-> number n
    (str n)
    (clojure.string/split n #"")
    (map #(Integer/parseInt %) n)))

(defn opcode [{memory :memory
               index :index }]
  (let [inst (nth memory index)]
    (->> (digits inst)
         (take-last 2)
         (apply str)
         (Integer/parseInt))))

(defn modes [memory index length]
  (let [digits (digits (nth memory index))
        modes (->> digits
                   (butlast)
                   (butlast))]
    (loop [modes modes]
      (if (= (count modes) length)
        modes
        (recur (conj modes 0))))))

(defn param [memory mode index]
  (if (= mode 0)
    (->> index (nth memory) (nth memory))
    (->> index (nth memory))))

(defn params [memory index nr]
  (->> (range (inc index) (+ 1 index nr))
       (map vector (reverse (modes memory index nr)))
       (map #(param memory (first %) (second %)))))

(defn handler [state steps f]
  (let [{memory :memory
         index :index} state
        inputs (params memory index (dec steps))
        output (param memory 1 (+ steps index))]
    (-> state
        (assoc-in [:memory output] (apply f inputs))
        (assoc :index (+ index (inc steps))))))

(defn jump-handler [state pred]
  (let [{memory :memory
         index :index} state
        [input-1 input-2] (params memory index 2)]
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
         input  :input} state
        output (param memory 1 (inc index))]
    (-> state
        (assoc-in [:memory output] (<!! input))
        (assoc :index (+ 2 index)))))

(defmethod intcode-computer 4 [state]
  (let [{memory :memory
         index  :index
         output :output} state
        mode   (first (modes memory index 1))
        result (param memory mode (inc index))]
    (>!! output result)
    (-> state
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

(defmethod intcode-computer 99 [state]
  (assoc state :finished true))

(def input (as-> (slurp "input.txt") in
             (clojure.string/trim-newline in)
             (clojure.string/split in #",")
             (map #(Integer/parseInt %) in)
             (into [] in)))

(defn run-intcode-computer [init-memory input]
  (let [output (chan)]
    [output
     (go-loop [state {:index 0 
                      :memory init-memory
                      :input input
                      :output output}]
              (if (:finished state)
                (:last-output state)
                (recur (intcode-computer state))))]))


(defn part-1 [memory]
  (apply max
         (for [a (range 0 5)
               b (range 0 5)
               c (range 0 5)
               d (range 0 5)
               e (range 0 5)
               :when (distinct? a b c d e)]
           (let [amp-a-in (chan)
                 amp-b-in (first (run-intcode-computer memory amp-a-in))
                 amp-c-in (first (run-intcode-computer memory amp-b-in))
                 amp-d-in (first (run-intcode-computer memory amp-c-in))
                 amp-e-in (first (run-intcode-computer memory amp-d-in))
                 out      (first (run-intcode-computer memory amp-e-in))]
             (go 
               (>! amp-a-in a)
               (>! amp-b-in b)
               (>! amp-c-in c)
               (>! amp-d-in d)
               (>! amp-e-in e)

               (>! amp-a-in 0))
             (<!! out)))))

(defn part-2 [memory]
  (apply max
         (for [a (range 5 10)
               b (range 5 10)
               c (range 5 10)
               d (range 5 10)
               e (range 5 10)
               :when (distinct? a b c d e)]
           (let [amp-a-in (chan)
                 amp-b-in (first (run-intcode-computer memory amp-a-in))
                 amp-c-in (first (run-intcode-computer memory amp-b-in))
                 amp-d-in (first (run-intcode-computer memory amp-c-in))
                 amp-e-in (first (run-intcode-computer memory amp-d-in))
                 [amp-e-out final-out] (run-intcode-computer memory amp-e-in)]
             (go 
               (>! amp-a-in a)
               (>! amp-b-in b)
               (>! amp-c-in c)
               (>! amp-d-in d)
               (>! amp-e-in e)

               (>! amp-a-in 0)
               (loop []
                 (>! amp-a-in (<! amp-e-out))
                 (recur)))

             (<!! final-out)))))


(println "Part 1:" (part-1 input))
(println "Part 2:" (part-2 input))

