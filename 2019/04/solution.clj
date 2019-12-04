(def input [134564 585159])

(defn two-adjacent-numbers-are-equal? [number]
  (not (nil? (re-matches #"\d*(\d)\1\d*" (str number)))))

(defn split-number [number]
  (as-> number num
    (str num)
    (clojure.string/split num #"")
    (map #(Integer/parseInt %) num)))


(defn digits-never-decrease? [number]
  (->> number
       (split-number)
       (reduce (fn [[valid prev] curr] 
                 [(and valid (>= curr prev)) curr])
               [true 0])
       (first)))

(defn is-valid-part-1? [number]
  (and (digits-never-decrease? number) 
       (two-adjacent-numbers-are-equal? number)))

(defn find-all-valid-numbers? [is-valid? start stop]
  (for [n (range start (inc stop))
        :when (is-valid? n)]
    n))

(println (str "Part 1: " (count 
                           (find-all-valid-numbers? 
                             is-valid-part-1?
                             (first input) 
                             (second input)))))

(defn exactly-two-adjacent-numbers-are-equal? [number]
  (let [digits (split-number number)]
    (loop [index -1]
      (let [digit-1 (nth digits index nil)
            digit-2 (nth digits (+ 1 index) nil)
            digit-3 (nth digits (+ 2 index) nil)
            digit-4 (nth digits (+ 3 index) nil)]
        (cond
          (and (nil? digit-3) (nil? digit-4)) false
          (and (=    digit-2 digit-3) 
               (not= digit-2 digit-1)
               (not= digit-2 digit-4)) true
          :else (recur (inc index)))))))

(defn is-valid-part-2? [number]
  (and (digits-never-decrease? number) 
       (exactly-two-adjacent-numbers-are-equal? number)))

(println (str "Part 1: " (count 
                           (find-all-valid-numbers? 
                             is-valid-part-2?
                             (first input) 
                             (second input)))))

