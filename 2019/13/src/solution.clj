(ns solution
  (:require
   [intcode.core :refer [run-program parse-file]]))

(def ins (parse-file "../13/input.txt"))

(println "Part 1:" (->> (run-program ins)
                        :output
                        (partition 3)
                        (filter #(= 2 (last %)))
                        count))

(defn update-game [{:keys [ball paddle]} {:keys [output]}]
  (let [find (fn [n] (->> output
                          (partition 3)
                          (filter #(= n (last %)))
                          last
                          first))]
    {:ball   (or (find 4) ball)
     :paddle (or (find 3) paddle)}))

(defn win-game []
  (loop [state (run-program (assoc ins 0 2N))
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
             last)))))

(println "Part 2:" (win-game))

