(ns sleipnir.kernel
  (require [clojure.core :as c]
           [clojure.string :as s]
           [clojure.walk :as w])
  (:refer-clojure :exclude [+ - / * mod < > <= >= = aget aset when while let]))

(defn cl-name [x]
  (s/replace x "-" "_"))

(defn ^:private fun [name xs]
  (str name "(" (s/join ", " xs) ")"))

(doseq [fun '[get-global-id sin cos sqrt pow]]
  (eval `(defn ~fun [~'& ~'xs] (fun ~(cl-name fun) ~'xs))))

(defn ^:private op [op xs]
  (str "(" (s/join " "
                   (if (c/= 1 (count xs))
                     (cons op xs)
                     (interpose op xs)))
       ")"))

(def ^:private renamed-ops '{mod %})
(doseq [op '[+ - / * mod < > <= >= =]]
  (eval `(defn ~op [~'& ~'xs] (op (get renamed-ops '~op '~op) ~'xs))))

(defn return [] "return")

(defn inc [x]
  (str x "++"))

(defn dec [x]
  (str x "--"))

(defn aget [a idx]
  (str a [idx]))

(defn aset [a idx val]
  (str a [idx] " = " val))

(defn do [& body]
  (str "{\n" (s/join
              (interleave (map #(str "  " %) body) (repeat ";\n")))
       "}"))

(defn if [test then & [else]]
  (str "if (" test ") "  then (c/when else (str " else " else))))

(defn when [test then]
  (sleipnir.kernel/if test then))

(defn while [test body]
  (str "while (" test ") " body ";"))

(defmacro let [[& bindings] & body]
  `(do (str (s/join ~(vec (for [[local value] (partition 2 bindings)]
                            `(str '~(-> local meta :tag)
                                  " " '~local " = " ~value ";\n"))))
            ~(c/let [vars (take-nth 2 bindings)
                   vars (vec (interleave vars (map #(list 'quote %) vars)))]
               `(c/let ~vars
                  (sleipnir.kernel/do ~@body))))))

(defmacro src [name args & body]
  `(str "kernel void " ~(cl-name name) " ("
        (s/join ", " ~(vec (for [arg args
                                 :let [t (-> arg meta :tag c/name)]]
                             `(str '~(c/when (c/= \* (last t)) "global ") '~(c/when-not (-> arg meta :out) "const ") '~t " " '~(cl-name arg)))))
        ") "
        ~(c/let [args (vec (interleave args (map #(list 'quote %) args)))]
           `(c/let ~args
              (sleipnir.kernel/do ~@(w/postwalk-replace (merge (zipmap args (map cl-name args))
                                                               (ns-interns 'sleipnir.kernel))
                                                        body))))))

(defmacro kernel [name args & body]
  `{:args '~args :src '~body :cl-src (src ~name ~args ~@body) :name '~name})