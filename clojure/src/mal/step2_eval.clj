(ns mal.step2-eval
  (:require [mal.reader :refer [read-str]]
            [mal.printer :refer [pri-forms]])
  (:import [jline.console ConsoleReader])
  (:gen-class))

(def repl-env {"+" +, "-" -, "*" *, "/" /})

(declare meval)

(defn eval-ast [{:keys [type val] :as ast} env]
  (case type
    :symbol (let [r (get env val ::not-found)]
              (if (= ::not-found r)
                (throw (ex-info (str "Could not resolve symbol: " (pr-str val))))
                r))
    :list {:type :list :val (mapv #(meval % env) val)}
    val))

(defn mread [x]
  (read-str x))

(defn meval [ast env]
  (if (= :list (:type ast))
    (let [lst (eval-ast ast env)
          [f & args] (:val lst)]
      (apply f args))
    (eval-ast ast env)))

(defn mprint [x]
  (pri-forms x))

(defn mrep [x]
  (try
    (-> x mread (meval repl-env) mprint)
    (catch clojure.lang.ExceptionInfo e
      (if (= :parse-error (-> e ex-data :type))
        (println (.getMessage e))
        (throw e)))))

(defn mrepl []
  (print "user> ")
  (flush)
  (if-let [input (read-line)]
    (do
      (println (mrep input))
      (recur))
    (do
      (newline)
      (println "EOF."))))

(defn repl []
  (if-let [input (-> (ConsoleReader.) (.readLine "user> "))]
    (do
      (println (mrep input))
      (recur))
    (do
      (newline)
      (println "EOF."))))

(defn -main []
  (repl))
