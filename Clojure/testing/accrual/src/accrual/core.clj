(ns accrual.core
  (:gen-class))

(def ledger {:transactions [{:time-allocated 0 :time-reported 0 :amount 1}
                            {:time-allocated 1 :time-reported 1 :amount 2}
                            {:time-allocated 2 :time-reported 1 :amount 1}
                            {:time-allocated 3 :time-reported 2 :amount 4}
                            {:time-allocated 4 :time-reported 2 :amount -3}
                            {:time-allocated 5 :time-reported 1 :amount -12}
                            {:time-allocated 7 :time-reported 4 :amount 10}
                            {:time-allocated 6 :time-reported 3 :amount 4}
                            {:time-allocated 8 :time-reported 2 :amount 8}]})

(def rate 0.03)

(defn get-empty-totals
  [ledger]
  (into [] (take (inc (reduce max (map :time-reported (:transactions ledger)))) (repeat 0))))

(defn day-totals
  [ledger]
  (reduce #(let [reported (:time-reported %2)
                   total (nth %1 reported)]
            (assoc %1 reported (+ total (:amount %2))))
          (get-empty-totals ledger)
          (:transactions ledger)))

(defn end-of-day-balances
  [ledger]
  (let [day-totals (day-totals ledger)]
      (loop [x 1
             acc []]
        (if (< x (inc (count day-totals)))
          (recur (inc x) (conj acc (reduce + (take x day-totals))))
          acc))))

(defn accrued-interest
  []
  (map #(* rate %) (end-of-day-balances ledger)))

(defn -main
  []
  (let [accruals (accrued-interest)]
    (println accruals)
    (println (reduce + accruals))))

