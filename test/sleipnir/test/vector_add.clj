(ns sleipnir.test.vector-add
  (use [sleipnir.core :only (defkernel run-kernel buffer-seq)])
  (import [com.jogamp.common.nio Buffers]))

(def element-count 1444477)

(defkernel vector-add [^double* a ^double* b ^:out ^double* c ^int num-elements]
  (let [^int iGID (get-global-id 0)]
    (when (< iGID num-elements)
      (aset c iGID (+ (aget a iGID) (aget b iGID))))))

(defn -main [& _]
  (let [a (doall (repeatedly element-count #(rand 100)))
        b (doall (repeatedly element-count #(rand 100)))]

    (time (println (take 10 (buffer-seq
                             (:c (vector-add a b
                                             (Buffers/newDirectDoubleBuffer (int element-count))
                                             element-count))))))

    (time (println (take 10 (buffer-seq
                             (:c (vector-add a b
                                             nil
                                             element-count))))))))