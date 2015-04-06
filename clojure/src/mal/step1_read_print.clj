(ns mal.step1-read-print
  (:import [jline.console ConsoleReader])
  (:gen-class))

(defn mread [x]
  x)
(defn meval [x]
  x)
(defn mprint [x]
  x)
(defn mrep [x]
  (-> x mread meval mprint))

(defn mrepl []
  (print "user> ")
  (flush)
  (if-let [input (read-line)]
    (do
      (println input)
      (recur))
    (do
      (newline)
      (println "EOF."))))

(defn repl []
  (if-let [input (-> (ConsoleReader.) (.readLine "user> "))]
    (do
      (println input)
      (recur))
    (do
      (newline)
      (println "EOF."))))

(defn -main []
  (repl))
