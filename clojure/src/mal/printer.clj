(ns mal.printer)

(defn pri-str
  [{:keys [type val] :as form}]
  (case type
    :number (str val)
    :symbol (str val)
    :string (pr-str val)
    :list (str "("
               (->> val
                    (map pri-str)
                    (clojure.string/join " "))
               ")")))

(defn pri-forms
  [forms]
  (->> forms
       (map pri-str)
       (clojure.string/join "\n")))
