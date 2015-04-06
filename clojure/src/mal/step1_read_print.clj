(ns mal.step1-read-print
  (:require [mal.reader :refer [read-str]]
            [mal.printer :refer [pri-forms]])
  (:import [jline.console ConsoleReader])
  (:gen-class))

(defn mread [x]
  (read-str x))
(defn meval [x]
  x)
(defn mprint [x]
  (pri-forms x))
(defn mrep [x]
  (try
    (-> x mread meval mprint)
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
