(defn parse-instruction [instruction]
  (let [opcode (mod instruction 100)
        mode-1 (mod (int (/ instruction 100)) 10)
        mode-2 (mod (int (/ instruction 1000)) 10)
        mode-3 (mod (int (/ instruction 10000)) 10)]
    {:opcode opcode
     :modes  (list mode-1 mode-2 mode-3)}))

(defn param
  ([state mode offset] (param state mode offset false))
  ([{memory        :memory
     index         :index
     relative-base :relative-base}
    mode
    offset
    output?]
   (cond->> (get memory (+ offset index))
     (= mode 2)                        (+ relative-base)
     (and (not output?) (not= mode 1)) (get memory)
     :always                           (#(or % 0)))))

(defn default-handler [state modes f]
  (let [index         (:index @state)
        [m-1 m-2 m-3] modes
        input-1       (param @state m-1 1)
        input-2       (param @state m-2 2)
        output        (param @state m-3 3 true)]
    (swap! state assoc-in [:memory output] (f input-1 input-2))
    (swap! state assoc :index (+ index 4))))

(defn jump-handler [state modes pred]
  (let [index     (:index @state)
        [m-1 m-2] modes
        input-1   (param @state m-1 1)
        input-2   (param @state m-2 2)]
    (swap! state assoc :index (if (pred input-1) input-2 (+ 3 index)))))

(defn run-intcode-computer [state]
  (let [state (atom (assoc state :output []))]
    (loop []
      (let [index (:index @state)
            {opcode :opcode
             modes  :modes} (parse-instruction (get-in @state [:memory index]))]
        (case opcode
          1  (default-handler state modes +)
          2  (default-handler state modes *)
          3  (let [index  (:index @state)
                   output (param @state (first modes) 1 true)]
               (swap! state assoc-in [:memory output] (:input @state))
               (swap! state assoc :index (+ index 2)))
          4  (let [{index :index} @state
                   [m]            modes
                   result         (param @state m 1)]
               (swap! state assoc :output (conj (:output @state) result))
               (swap! state assoc :index (+ 2 index)))
          5  (jump-handler state modes #(not= 0 %))
          6  (jump-handler state modes #(= 0 %))
          7  (default-handler state modes #(if (< %1 %2) 1 0))
          8  (default-handler state modes #(if (= %1 %2) 1 0))
          9  (let [{index         :index
                    relative-base :relative-base} @state
                   result (param @state (first modes) 1)]
               (swap! state assoc :relative-base (+ relative-base result))
               (swap! state assoc :index (+ 2 index)))
          99 (swap! state assoc :finished true))

        (if (or (= 2 (count (:output @state))) (:finished @state))
          @state
          (recur))))))

(def input
  (as-> (slurp "input.txt") in
        (clojure.string/trim-newline in)
        (clojure.string/split in #",")
        (map #(bigint %) in)
        (map-indexed #(do [%1 %2]) in)
        (into {} in)))

(def init-state
  {:index         0
   :relative-base 0
   :memory        input
   :input         0N})

(defn turn-left [direction]
  (case direction
    [0 1]  [-1 0]
    [-1 0] [0 -1]
    [0 -1] [1 0]
    [1 0]  [0 1]))

(defn turn-right [direction]
  (case direction
    [0 1]  [1 0]
    [1 0]  [0 -1]
    [0 -1] [-1 0]
    [-1 0] [0 1]))


(defn ship-paint [init-state]
  (loop [state     (run-intcode-computer init-state)
         direction [0 1]
         position  [0 0]
         ship      {}]
    (let [[color turn]  (:output state)
          new-ship      (assoc ship position color)
          new-direction (if (= turn 0)
                          (turn-left direction)
                          (turn-right direction))
          new-position  (map + position new-direction)
          new-state     (assoc state :input (or (get ship new-position) 0N))]
      (if (:finished state)
        ship
        (recur
          (run-intcode-computer new-state)
          new-direction
          new-position
          new-ship)))))

(println "Part 1:" (count (ship-paint init-state)))

(def paint (ship-paint (assoc init-state :input 1N)))

(println "Part 2:")
(doall
 (for [y (reverse (range -10 5))]
   (do
     (doall
      (for [x  (range -10 50)]
        (print (if (= (get paint [x y]) 1) "#" " "))))
     (println))))
