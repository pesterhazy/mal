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
  (-> x mread meval mprint))

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
