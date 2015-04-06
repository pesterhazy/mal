(ns mal.step0-repl
  (:gen-class))

(defn mread [x]
  x)
(defn meval [x]
  x)
(defn mprint [x]
  x)
(defn mrep [x]
  (-> x mread meval mprint))

(defn main-loop []
  (print "user> ")
  (flush)
  (if-let [input (read-line)]
    (do
      (println input)
      (recur))
    (do
      (newline)
      (println "EOF."))))

(defn -main []
  (main-loop))
