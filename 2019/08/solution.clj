(def input (as-> 
             (slurp "input.txt") in
             (clojure.string/split in #"")
             (map #(Integer/parseInt %) in)))

(defn chunked-list [w l]
  (reduce (fn [a x] 
            (if (> w (count (last a)))
              (conj (pop  a) (conj (last a) x))
              (conj a [x])))
          [[]]
          l
          ))

(defn count-n [n l]
  (->> l
       (filter #(= n %))
       (count)))

(println "Part 1:" (->> input
                        (chunked-list (* 25 6))
                        (map #(vector (count-n 0 %) %))
                        (apply min-key first)
                        (second)
                        (#(* (count-n 1 %)
                             (count-n 2 %)))))

(defn render-px [ & rest]
  (reduce (fn [a b] (if (not= a 2)
                      a
                      b))
          rest)) 

(defn render! [base w l]
  (doall
    (->> base
         (chunked-list (* w l))
         (apply map render-px)
         (map #(if (= 1 %) "▮" " "))
         (chunked-list w)
         (map #(apply str %))
         (map println)
    )))


(println "Part 2:")
(render! input 25 6)

