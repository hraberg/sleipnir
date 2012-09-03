(ns sleipnir.core
  (require [clojure.string :as s]
           [clojure.walk :as w]
           [sleipnir.kernel :as k])
  (import [com.jogamp.opencl CLDevice CLBuffer CLContext CLKernel CLProgram CLResource CLCommandQueue CLMemory$Mem]
          [com.jogamp.common.nio Buffers]
          [java.nio DoubleBuffer ByteBuffer ByteOrder Buffer]))

;; (set! *warn-on-reflection* true)

(defonce ^CLContext context (CLContext/create))
(def ^CLDevice device (.getMaxFlopsDevice context))
(def ^CLCommandQueue queue (.createCommandQueue device))

(def local-work-size (min (.getMaxWorkGroupSize device) 256))

(defn ^:private round-up [group global]
  (let [r (mod global group)]
    (if (zero? r)
      global
      (+ global (- group r)))))

(defmacro ^:private buffer-seq-switch [^Buffer b]
  `(condp instance? ~'b
           ~@(reduce into []
                     (for [t '[Double Byte Long Int Float Short]
                           :let [bn (symbol (str "java.nio." t "Buffer"))
                                 b (with-meta 'b {:tag bn})]]
                       [bn
                        `(let [a# (~(symbol (str (s/lower-case t) "-array")) (.limit ~b))]
                           (.get ~b a#)
                           a#)]))))

(defn buffer-seq [b]
  (buffer-seq-switch b))

(defmacro ^:private put-arguments [a ra size read-buffers write-buffers ^CLKernel kernel]
  `(case (-> ~a meta :tag)
     ~@(reduce into []
               (for [t '[Double Byte Long Int Float Short]
                     :let [bn (symbol (str "java.nio." t "Buffer"))
                           cb (symbol (str ".create" t "Buffer")) ]]
                 [(symbol (str (s/lower-case t) "*"))
                  `(if ((meta ~a) :out)
                     (let [^CLBuffer b# (if (instance? Buffer ~ra)
                                          (.createBuffer ^CLContext context ^Buffer ~ra nil)
                                          (~cb context ~size (into-array [CLMemory$Mem/READ_ONLY])))]
                       (swap! ~read-buffers conj b#)
                       (.putArg ~kernel b#))
                     (let [^CLBuffer b# (~cb context ~size (into-array [CLMemory$Mem/WRITE_ONLY]))
                           bb# (.getBuffer b#)]
                       (swap! ~write-buffers conj b#)
                       (.putArg ~kernel b#)
                       (.put bb# (if (.isArray (class ~ra)) ~ra
                                     (~(symbol (str (s/lower-case t) "-array")) ~size ~ra)))
                       (.rewind bb#)
                       (.putWriteBuffer queue b# false)))]))
     (.putArg ~kernel ((resolve (-> ~a meta :tag)) ~ra))))

(defn ^:private  build-kernel [^String cl-src]
  (.build (.createProgram context cl-src)))
(alter-var-root #'build-kernel memoize)

(defn run-kernel [kernel & real-args]
  (let [{:keys [src name args cl-src]} kernel
        ^CLProgram program (build-kernel cl-src)
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
        (put-arguments a ra size read-buffers write-buffers kernel))
      (.put1DRangeKernel queue kernel 0 size local-work-size)
      (doseq [b @read-buffers]
        (.putReadBuffer queue b true))
      (zipmap (map keyword (filter (comp :out meta) args))
              (map #(buffer-seq (.getBuffer ^CLBuffer %)) @read-buffers))
      (finally
        (dorun (map #(.release ^CLResource %) (concat [kernel] @read-buffers @write-buffers)))))))

(defmacro defkernel [name args & body]
  `(defn ~name ~(vec (map #(with-meta % {}) args))
     (run-kernel (sleipnir.kernel/kernel ~name ~args ~@body)
                 ~@args)))

;; (defn mapk [kernel]
;;   (let [{:keys [name src args]} (meta kernel)
;;         as-array #(with-meta % {:tag (symbol (str (-> % meta :tag) "*"))})
;;         body `(k/let [~(with-meta 'iGID {:tag 'int}) (k/get-global-id 0)]
;;                 (k/when (k/< ~'iGID ~'num-elements)
;;                   (k/aset ~'output ~'iGID (k/do ~@(w/postwalk-replace (zipmap args (map #(list 'k/aget % 'iGID) args)) src)))))
;;         args (vec (concat (map as-array args) [(as-array (with-meta 'output {:tag (-> args meta :tag :out)}))
;;                                 (with-meta 'num-elements {:tag 'int})]))]
;;     (eval `(k/kernel ~name ~args ~@body))))
