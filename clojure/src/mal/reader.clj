(ns mal.reader
  (require [clojure.edn :as edn])
  )

(declare read-form)

(defn tokenize
  [s]
  (->> s
       (re-seq #"[\s,]*(~@|[\[\]{}()'`~^@]|\"(?:\\.|[^\\\"])*\"|;.*|[^\s\[\]{}('\"`,;)]*)")
       (map second)
       (keep #(when-not (empty? %) %))))

(defn read-list [[_ & tokens]]
  (loop [[token & rst :as tokens] tokens
         xs []]
    (if (= ")" token)
      [{:type :list, :val xs} rst]
      (let [[x rst] (read-form tokens)]
        (recur rst (conj xs x))))))

(defn is-symbol? [st] (re-find #"^[a-zA-Z+*/-][\w'+*/-]*$" st))
(defn is-string? [st] (re-find #"^\".*\"$" st))

(defn parse-num [st]
  (try
    (Integer/parseInt st)
    (catch NumberFormatException e
      (try
        (Double/parseDouble st)
        (catch NumberFormatException e
          nil)))))

(defn read-atom [[token & more]]
  [(cond (is-symbol? token) {:type :symbol, :val token}
         (is-string? token) {:type :string, :val (edn/read-string token)}
         :else (if-let [num (parse-num token)]
                 {:type :number, :val num}
                 (throw (IllegalArgumentException. (str "Failed to parse token "
                                                        (pr-str token))))))
   more])

(defn read-quote [[token & more]]
  (let [[form remaining-tokens] (read-form more)]
    [{:type :list, :val [{:type :symbol :val "quote"} form]}
     remaining-tokens]))

(defn read-form
  "Read a form. Returns a pair of [form remaining-tokens]"
  [[token & rst :as tokens]]
  (cond (= "(" token) (read-list tokens)
        (= "'" token) (read-quote tokens)
        :else (read-atom tokens)))

(defn read-forms
  [tokens]
  (loop [tokens tokens
         forms []]
    (if tokens
      (let [[form remaining] (read-form tokens)]
        (recur remaining (conj forms form)))
      forms)))

(defn read-str
  [st]
  (-> st tokenize read-forms))
