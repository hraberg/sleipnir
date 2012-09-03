(ns sleipnir.test.vector-add
  (use [sleipnir.core :only (defkernel device)]))

(def element-count 1444477)

(defkernel vector-add [^float* a ^float* b ^:out ^float* c ^int num-elements]
  (let [^int iGID (get-global-id 0)]
    (when (< iGID num-elements)
      (aset c iGID (+ (aget a iGID) (aget b iGID))))))

(defn -main [& _]
  (let [a (float-array element-count (repeatedly #(rand 100)))
        b (float-array element-count (repeatedly #(rand 100)))]

    (dotimes [n 3]
      (println)

      (println (str device))
      (time (println (->> (vector-add a b :c element-count)
                          :c (take 10))))

      (println "Clojure" (clojure-version))
      (time (println (take 10 (loop [i 0
                                     acc (float-array element-count)]
                                (if (= i element-count)
                                  acc
                                  (do
                                    (aset acc i (+ (aget a i) (aget b i)))
                                    (recur (inc i) acc))))))))))