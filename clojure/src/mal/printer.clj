(ns mal.printer)

(declare pri-str)

(defn pri-str-coll
  [opening closing xs]
  (str opening
       (->> xs (map pri-str) (clojure.string/join " "))
       closing))

(defn pri-str
  [{:keys [type val] :as form}]
  (case type
    :number (str val)
    :symbol (str val)
    :keyword (str ":" val)
    :string (pr-str val)
    :list (pri-str-coll "(" ")" val)
    :vector (pri-str-coll "[" "]" val)
    :hash-map (pri-str-coll "{" "}" val)
    ))

(defn pri-forms
  [forms]
  (->> forms
       (keep identity)
       (map pri-str)
       (clojure.string/join "\n")))
