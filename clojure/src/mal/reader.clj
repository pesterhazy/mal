(ns mal.reader)

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

(defn parse-num [st]
  (try
    (Integer/parseInt st)
    (catch NumberFormatException e
      (try
        (Double/parseDouble st)
        (catch NumberFormatException e
          nil)))))

(defn read-atom [[token & more]]
  [(if (is-symbol? token)
     {:type :symbol, :val token}
     (if-let [num (parse-num token)]
       {:type :number, :val num}
       (throw (IllegalArgumentException. (str "Failed to parse token "
                                              (pr-str token))))))
   more])

(defn read-form
  "Read a form. Returns a pair of [form remaining-tokens]"
  [[token & rst :as tokens]]
  (if (= "(" token)
    (read-list tokens)
    (read-atom tokens)))

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
