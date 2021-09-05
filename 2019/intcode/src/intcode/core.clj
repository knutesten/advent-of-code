(ns intcode.core
  (:require
   [clojure.core.async :as a]
   [clojure.string :as str]))

(defn parse-int [s]
  (bigint s))

(defn parse-instructions [s]
  (mapv parse-int (str/split s #",")))

(defn parse-file [path]
  (parse-instructions (str/trim (str/trim-newline (slurp path)))))

(defn chan? [ch]
  (instance? clojure.core.async.impl.channels.ManyToManyChannel
             ch))

(defn parse-opcode [{:keys [ins pos]}]
  (let [code (ins pos)
        op   (rem code 100)
        m1   (rem (quot code 100) 10)
        m2   (rem (quot code 1000) 10)
        m3   (rem (quot code 10000) 10)]
    [op m1 m2 m3]))

(defn current-opcode [state]
  (first (parse-opcode state)))

(defn current-params [{:keys [ins pos base] :as state}
                      & {:keys [writes? param-count]
                         :or   {param-count 0}}]
  (let [[op & modes] (parse-opcode state)
        modes        (vec (take param-count modes))
        modes        (if writes? (update modes (dec (count modes)) + 10) modes)
        params       (map ins (take (count modes) (iterate inc (inc pos))))]
    (mapv (fn [mode param]
            (case mode
              0  (get ins param 0)
              1  param
              2  (get ins (+ base param) 0)
              10 param
              12 (+ base param)))
          modes
          params)))

(defmulti compute current-opcode)

(defmethod compute 1 [state]
  (let [[a b p] (current-params state
                                :param-count 3
                                :writes? true)]
    (-> state
        (update :ins assoc p (+ a b))
        (update :pos + 4))))

(defmethod compute 2 [state]
  (let [[a b p] (current-params state
                                :param-count 3
                                :writes? true)]
    (-> state
        (update :ins assoc p (* a b))
        (update :pos + 4))))

(defmethod compute 3 [{:keys [input] :as state}]
  (let [[p] (current-params state
                            :param-count 1
                            :writes? true)
        in  (if (chan? input)
              (a/<!! input)
              (peek input))]
    (if (nil? in)
      (assoc state :paused? true)
      (cond-> state
        (vector? input) (update :input pop)
        true            (update :ins assoc p in)
        true            (update :pos + 2)))))

(defmethod compute 4 [{:keys [output] :as state}]
  (let [[out] (current-params state
                              :param-count 1)]
    (when (chan? output)
      (a/>!! output out))
    (cond-> state
      (not (chan? output)) (update :output (fnil conj []) out)
      (chan? output)       (assoc :final-output out)
      true                 (update :pos + 2))))

(defmethod compute 5 [state]
  (let [[p1 p2] (current-params state
                                :param-count 2)]
    (if (zero? p1)
      (update state :pos + 3)
      (assoc state :pos p2))))

(defmethod compute 6 [state]
  (let [[p1 p2] (current-params state
                                :param-count 2)]
    (if (zero? p1)
      (assoc state :pos p2)
      (update state :pos + 3))))

(defmethod compute 7 [state]
  (let [[p1 p2 p3] (current-params state
                                   :writes? true
                                   :param-count 3)
        v          (if (< p1 p2) 1 0)]
    (-> state
        (update :ins assoc p3 v)
        (update :pos + 4))))

(defmethod compute 8 [state]
  (let [[p1 p2 p3] (current-params state
                                   :writes? true
                                   :param-count 3)
        v          (if (= p1 p2) 1 0)]
    (-> state
        (update :ins assoc p3 v)
        (update :pos + 4))))

(defmethod compute 9 [state]
  (let [[p1] (current-params state
                             :param-count 1)]
    (-> state
        (update :base + p1)
        (update :pos + 2))))

(defmethod compute 99 [state]
  (assoc state :finished? true))

(defn run-program [ins & {:keys [input] :or {input []}}]
  (try
    (let [init-state (if (vector? ins)
                       {:ins   (into {} (map-indexed vector ins))
                        :pos   0
                        :base  0
                        :input input}
                       (-> ins
                           (dissoc :output)
                           (assoc :input input)))]
      (if (chan? input)
        (let [output (a/chan)]
          [output
           (a/go-loop [state (assoc init-state :output output)]
             (if (= 99 (current-opcode state))
               (:final-output state)
               (recur (compute state))))])
        (loop [state init-state]
          (if (or (:paused? state)
                  (:finished? state))
            (dissoc state :paused?)
            (recur (compute state))))))
    (catch Exception e
      (println e))))

