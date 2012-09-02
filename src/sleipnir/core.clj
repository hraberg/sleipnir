(ns sleipnir.core
  (require [clojure.string :as s]
           [clojure.walk :as w]
           [sleipnir.kernel :as cl])
  (import [com.jogamp.opencl CLDevice CLBuffer CLContext CLKernel CLCommandQueue]
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

(defn run-kernel [kernel & real-args]
  (let [{:keys [src name args]} (meta kernel)
        program (.build (.createProgram context ^String (kernel)))
        ^CLKernel kernel (.createCLKernel program (cl/cl-name name))
        read-buffers (atom [])
        size (round-up local-work-size (apply max (map count (filter sequential? real-args))))]
    (doseq [[a ra] (partition 2 (interleave args real-args))
            :let [type (-> a meta :tag)
                  mode (or (some #{:out} (keys (meta a))) :w)]]
      (case type
        'double* (if (= :out mode)
                   (let [^CLBuffer b (if ra (.createBuffer context ^Buffer ra nil) (.createDoubleBuffer context size nil))]
                     (swap! read-buffers conj b)
                     (.putArg kernel b))
                   (let [^CLBuffer b (.createDoubleBuffer context size nil)
                         ^DoubleBuffer db (.getBuffer b)]
                     (.putArg kernel b)
                     (.put db (double-array size ra))
                     (.rewind db)
                     (.putWriteBuffer queue b false)))
        (.putArg kernel ra)))
    (.rewind kernel)
    (.put1DRangeKernel queue kernel 0 size local-work-size)
    (when (seq @read-buffers)
      (doseq [b (butlast @read-buffers)]
        (.putReadBuffer queue b false))
      (.putReadBuffer queue (last @read-buffers) true))
    (zipmap (map keyword (filter #(-> % meta :out) args)) (map #(.getBuffer ^CLBuffer %) @read-buffers))))

;; (defn mapk [kernel]
;;   (let [{:keys [name src args]} (meta kernel)
;;         as-array #(with-meta % {:tag (symbol (str (-> % meta :tag) "*"))})
;;         body `(cl/let [~(with-meta 'iGID {:tag 'int}) (cl/get-global-id 0)]
;;                 (cl/when (cl/< ~'iGID ~'num-elements)
;;                   (cl/aset ~'output ~'iGID (cl/do ~@(w/postwalk-replace (zipmap args (map #(list 'cl/aget % 'iGID) args)) src)))))
;;         args (vec (concat (map as-array args) [(as-array (with-meta 'output {:tag (-> args meta :tag :out)}))
;;                                 (with-meta 'num-elements {:tag 'int})]))]
;;     (eval `(cl/kernel ~name ~args ~@body))))

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
