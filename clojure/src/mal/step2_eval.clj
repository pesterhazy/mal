(ns mal.step2-eval
  (:require [mal.reader :refer [read-str]]
            [mal.printer :refer [pri-str]])
  (:import [jline.console ConsoleReader])
  (:gen-class))

(def repl-env {"+" +, "-" -, "*" *, "/" /})

(declare meval)

(defn eval-ast [{:keys [type val] :as ast} env]
  (case type
    :symbol (let [r (get env val ::not-found)]
              (if (= ::not-found r)
                (throw (ex-info (str "Could not resolve symbol: " (pr-str val))
                                {:type :compile-error}))
                r))
    :list (mapv #(meval % env) val)
    :vector (mapv #(meval % env) val)
    :hash-map (apply hash-map (map #(meval % env) val))
    val))

(defn mread [x]
  (read-str x))

(defn meval [ast env]
  (if (= :list (:type ast))
    (let [[f & args] (eval-ast ast env)]
      (apply f args))
    (eval-ast ast env)))

(defn mprint [x]
  (pr-str x))

(defn mrep [x]
  (try
    (-> x mread (meval repl-env) mprint)
    (catch clojure.lang.ExceptionInfo e
      (let [type (-> e ex-data :type)]
        (if (#{:parse-error :compile-error} type)
          (println {:parse-error "Parse error",
                    :compile-error "Compile error"}
                   " "
                   (.getMessage e))
          (throw e))))))

(defn mrepl []
  (print "mal-user> ")
  (flush)
  (if-let [input (read-line)]
    (do
      (println (mrep input))
      (recur))
    (do
      (newline)
      (println "EOF."))))

(defn repl []
  (if-let [input (-> (ConsoleReader.) (.readLine "mal-user> "))]
    (do
      (println (mrep input))
      (recur))
    (do
      (newline)
      (println "EOF."))))

(defn -main []
  (repl))
