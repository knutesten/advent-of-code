(ns intcode.core-test
  (:require
   [intcode.core :refer [run-program parse-file]]
   [clojure.core.async :as a]
   [clojure.test :refer :all]))

(deftest day-2
  (let [inst   (parse-file "../02/input.txt")
        calc   (fn [n v]
                 (second (first (:ins (run-program
                                        (-> inst
                                            (assoc 1 n)
                                            (assoc 2 v)))))))
        part-1 (calc 12 2)
        part-2 (->> (for [n (range 100) v (range 100)] [n v])
                    (drop-while #(not= 19690720 (apply calc %)))
                    first
                    ((fn [[n v]] (+ (* 100 n) v))))]
    (testing "Day 2 - Part 1"
      (is (= 3760627 part-1)))

    (testing "Day 2 - Part 2"
      (is (= 7195 part-2)))))

(deftest day-5
  (let [inst (parse-file "../05/input.txt")]
    (testing "Day 5 - Part 1"
      (is (= 9219874 (last (:output (run-program inst :input [1]))))))

    (testing "Day 5 - Part 2"
      (is (= 5893654 (last (:output (run-program inst :input [5]))))))))

(deftest day-7
  (let [ins             (parse-file "../07/input.txt")
        phase-seq       (for [a     (range 5)
                              b     (range 5)
                              c     (range 5)
                              d     (range 5)
                              e     (range 5)
                              :when (distinct? a b c d e)]
                          [a b c d e])
        amp             #(-> (run-program ins :input [%2 %1])
                             :output
                             first)
        amps            #(reduce (fn [out ph] (amp ph out))
                                 0
                                 %1)
        phase-async-seq (for [a     (range 5 10)
                              b     (range 5 10)
                              c     (range 5 10)
                              d     (range 5 10)
                              e     (range 5 10)
                              :when (distinct? a b c d e)]
                          [a b c d e])
        amps-async      (fn [phases]
                          (let [a-in        (a/chan)
                                prog-seq    (iterate
                                              #(run-program ins :input (first %))
                                              (run-program ins :input a-in))
                                progs       (take 5 prog-seq)
                                [e-out out] (last progs)]
                            (a/go
                              (a/pipe e-out a-in)
                              (a/>! a-in (first phases))
                              (doseq [[i [p x]] (map vector
                                                     (iterate inc 1)
                                                     (butlast progs))]
                                (a/>! p (nth phases i)))
                              (a/>! a-in 0))
                            (a/<!! out)))
        part-2          (->> phase-async-seq
                             (map amps-async)
                             (reduce max))
        part-1          (->> phase-seq
                             (map amps)
                             (reduce max))]
    (testing "Day 7 - Part 1"
      (is (= 38834 part-1)))
    (testing "Day 7 - Part 2"
      (is (= 69113332 part-2)))))

(deftest day-9
  (let [ins    (parse-file "../09/input.txt")
        part-1 (first (:output (run-program ins :input [1])))
        part-2 (first (:output (run-program ins :input [2])))]
    (testing "Day 9 - Part 1"
      (is (= 2752191671N part-1)))

    (testing "Day 9 - Part 2"
      (is (= 87571N part-2)))))

(deftest day-13
  (let [ins         (parse-file "../13/input.txt")
        part-1      (->> (run-program ins)
                         :output
                         (partition 3)
                         (filter #(= 2 (last %)))
                         count)
        ins         (assoc ins 0 2N)
        update-game (fn [{:keys [ball paddle]} {:keys [output]}]
                      (let [find (fn [n] (->> output
                                              (partition 3)
                                              (filter #(= n (last %)))
                                              last
                                              first))]
                        {:ball   (or (find 4) ball)
                         :paddle (or (find 3) paddle)}))
        part-2      (loop [state (run-program ins)
                           game  {:paddle nil
                                  :ball   nil}]
                      (let [{:keys [ball paddle]
                             :as   game} (update-game game state)
                            dir          (cond
                                           (< ball paddle) -1
                                           (> ball paddle) 1
                                           :else           0)]
                        (if-not (:finished? state)
                          (recur (run-program state :input [dir]) game)
                          (->> state
                               :output
                               (partition 3)
                               (filter #(= -1 (first %)))
                               last
                               last))))]
    (testing "Day 13 - Part 1"
      (is (= 280 part-1)))

    (testing "Day 13 - Part 2"
      (is (= 13298 part-2)))))

