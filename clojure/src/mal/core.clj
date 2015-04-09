(ns mal.core)

(defrecord MalKeyword [name])

(defmethod clojure.core/print-method MalKeyword [x writer]
  (.write writer (str ":" (:name x))))
