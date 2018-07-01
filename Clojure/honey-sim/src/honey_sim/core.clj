(ns honey-sim.core
  (:gen-class))

(def starting-stats
  {:bear 6
   :criminal 6})

(defn die []
  (+ 1 (rand-int 6)))

(defn test-attr
  [attr stats]
  (let [val (attr stats)]
    (if (< (die) val)
      stats
      (assoc stats attr (- val 1)))))

(defn has-zero?
  [stats]
  (or (zero? (:bear stats))
      (zero? (:criminal stats))))

(defn rand-attr []
  (if (= 0 (rand-int 1))
    :bear
    :criminal))

(defn run []
  (loop [stats starting-stats
         i 0]
    (if (has-zero? stats)
      i
      (recur (test-attr (rand-attr) stats)
             (+ i 1)))))

(defn average
  [results]
  (/ (apply + results) (count results)))

(defn -main
  [& args]
  (let [n 10000000]
    (printf "running %s times: " n)
    (println (float (average (take n (repeatedly run)))))))
