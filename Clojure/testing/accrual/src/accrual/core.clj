(ns accrual.core
  (:gen-class))

;;;; Basic accrual model

;;; Transactions in the ledger have a :transaction-date which represents when the transaction actually occurred
;;; They also have a :allocation-date which represents when they were allocated to an account
;;; The :allocation-date is not used and is only here to show that the time the transactions came into the ledger is unimportant
(def ledger {:transactions [{:allocation-date 0 :transaction-date 0 :amount 1}
                            {:allocation-date 1 :transaction-date 1 :amount 2}
                            {:allocation-date 2 :transaction-date 1 :amount 1}
                            {:allocation-date 3 :transaction-date 2 :amount 4}
                            {:allocation-date 4 :transaction-date 2 :amount -3}
                            {:allocation-date 5 :transaction-date 1 :amount -12}
                            {:allocation-date 7 :transaction-date 4 :amount 10}
                            {:allocation-date 6 :transaction-date 3 :amount 4}
                            {:allocation-date 8 :transaction-date 2 :amount 8}
                            {:allocation-date 9 :transaction-date 6 :amount 3}]})

;; List of historical interest rates
(def rates [{:effective-date 0 :value 0.03}
            {:effective-date 3 :value 0.04}])

(defn get-empty-totals
  "Get a vector of 0s big enough to allow one index for each possible :time-reported value (i.e. one per day)"
  [transactions]
  (into [] (take
            (inc (reduce max (map :transaction-date transactions)))
            (repeat 0))))

(defn day-totals
  "Calculate the total added to the account each day"
  [transactions]
  (reduce #(let [reported (:transaction-date %2)
                   total (nth %1 reported)]
            (assoc %1 reported (+ total (:amount %2))))
          (get-empty-totals transactions)
          transactions))

(defn end-of-day-balances
  "Calculate the end of day balances as the previous balance plus the total for the day"
  [transactions]
  (let [day-totals (day-totals transactions)]
      (loop [x 1
             acc []]
        (if (< x (inc (count day-totals)))
          (recur (inc x) (conj acc (reduce + (take x day-totals))))
          acc))))

(defn get-rate
  "Get the appropriate rate for a date"
  [date]
  (:value (last (filter #(<= (:effective-date %) date) rates))))

(defn accrued-interest
  "Calculate the interest accrued on the end of day balances for each day"
  []
  (map-indexed
   (fn [i value] (* (get-rate i) value))
   (end-of-day-balances (:transactions ledger))))

(defn -main
  "Calculate and print the interest accrued each day and the sum total"
  []
  (let [accruals (accrued-interest)]
    (println accruals)
    (println (reduce + accruals))))

