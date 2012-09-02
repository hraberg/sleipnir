(ns sleipnir.test.vector-add
  (use [sleipnir.core :only (defkernel run-kernel buffer-seq)])
  (import [com.jogamp.common.nio Buffers]))

(def element-count 1444477)

(defkernel vector-add [^double* a ^double* b ^:out ^double* c ^int num-elements]
  (let [^int iGID (get-global-id 0)]
    (when (< iGID num-elements)
      (aset c iGID (+ (aget a iGID) (aget b iGID))))))

(defn -main [& _]
  (let [a (double-array (repeatedly element-count #(rand 100)))
        b (double-array (repeatedly element-count #(rand 100)))]

    (time (println (take 10 (buffer-seq
                             (:c (vector-add a b
                                             nil
                                             element-count))))))

    (time (println (take 10 (loop [i 0
                                   acc (double-array element-count)]
                              (if (= i element-count)
                                acc
                                (do
                                  (aset acc i (+ (aget a i) (aget b i)))
                                  (recur (inc i) acc)))))))))