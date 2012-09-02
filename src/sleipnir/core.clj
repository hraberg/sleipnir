(ns sleipnir.core
  (require [clojure.string :as s]
           [clojure.walk :as w]
           [sleipnir.kernel :as k])
  (import [com.jogamp.opencl CLDevice CLBuffer CLContext CLKernel CLCommandQueue CLMemory$Mem]
          [com.jogamp.common.nio Buffers]
          [java.nio DoubleBuffer ByteBuffer ByteOrder Buffer]))

(set! *warn-on-reflection* true)

(def ^CLContext context (CLContext/create))
(def ^CLDevice device (.getMaxFlopsDevice context))
(def ^CLCommandQueue queue (.createCommandQueue device))

(defn round-up [group global]
  (let [r (mod global group)]
    (if (zero? r)
      global
      (+ global (- group r)))))

(def local-work-size (min (.getMaxWorkGroupSize device) 256))

(defn build [kernel]
  (.build (.createProgram context kernel)))
(alter-var-root #'build memoize)

(defn run-kernel [kernel & real-args]
  (let [{:keys [src name args ^String cl-src]} kernel
        program (build cl-src)
        ^CLKernel kernel (.createCLKernel program (k/cl-name name))
        read-buffers (atom [])
        write-buffers (atom [])
        size (round-up local-work-size  (->> real-args
                                             (filter (some-fn sequential?
                                                              #(and % (.isArray (class %)))))
                                             (map count)
                                             (apply max)))]
    (try
      (doseq [[a ra] (partition 2 (interleave args real-args))]
        (case (-> a meta :tag)
          'double* (if ((meta a) :out)
                     (let [^CLBuffer b (if ra (.createBuffer context ^Buffer ra nil) (.createDoubleBuffer context size
                                                                                                          (into-array [CLMemory$Mem/READ_ONLY])))]
                       (swap! read-buffers conj b)
                       (.putArg kernel b))
                     (let [^CLBuffer b (.createDoubleBuffer context size
                                                            (into-array [CLMemory$Mem/WRITE_ONLY]))
                           ^DoubleBuffer db (.getBuffer b)]
                       (swap! write-buffers conj b)
                       (.putArg kernel b)
                       (.put db (if (.isArray (class ra)) ra (double-array size ra)))
                       (.rewind db)
                       (.putWriteBuffer queue b false)))
          (.putArg kernel ra)))
      (.put1DRangeKernel queue kernel 0 size local-work-size)
      (when (seq @read-buffers)
        (doseq [b (butlast @read-buffers)]
          (.putReadBuffer queue b false))
        (.putReadBuffer queue (last @read-buffers) true))
      (zipmap (map keyword (filter #(-> % meta :out) args)) (map #(.getBuffer ^CLBuffer %) @read-buffers))
      (finally
        (.release kernel)
        (dorun (map #(.release %) (concat @read-buffers @write-buffers)))))))

;; (defn mapk [kernel]
;;   (let [{:keys [name src args]} (meta kernel)
;;         as-array #(with-meta % {:tag (symbol (str (-> % meta :tag) "*"))})
;;         body `(k/let [~(with-meta 'iGID {:tag 'int}) (k/get-global-id 0)]
;;                 (k/when (k/< ~'iGID ~'num-elements)
;;                   (k/aset ~'output ~'iGID (k/do ~@(w/postwalk-replace (zipmap args (map #(list 'k/aget % 'iGID) args)) src)))))
;;         args (vec (concat (map as-array args) [(as-array (with-meta 'output {:tag (-> args meta :tag :out)}))
;;                                 (with-meta 'num-elements {:tag 'int})]))]
;;     (eval `(k/kernel ~name ~args ~@body))))

(defmacro defkernel [name args & body]
  `(defn ~name ~(vec (map #(with-meta % {}) args))
     (run-kernel (sleipnir.kernel/kernel ~name ~args ~@body)
                 ~@args)))

(eval `(defn buffer-seq [^Buffer ~'b]
         (condp instance? ~'b
           ~@(reduce into []
                     (for [t '[Double Byte Long Int]
                           :let [bn (symbol (str "java.nio." t "Buffer"))
                                 b (with-meta 'b {:tag bn})]]
                       [bn
                        `(let [a# (~(symbol (str (s/lower-case t) "-array")) (.limit ~b))]
                           (.get ~b a#)
                           a#)])))))
