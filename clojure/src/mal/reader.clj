(ns mal.reader
  (require [clojure.edn :as edn]
           [mal.core :refer [->MalKeyword]]))

(declare read-form)

(defn tokenize
  [s]
  (->> s
       (re-seq #"[\s,]*(~@|[\[\]{}()'`~^@]|\"(?:\\.|[^\\\"])*\"|;.*|[^\s\[\]{}('\"`,;)]*)")
       (map second)
       (keep #(when-not (empty? %) %))))

(def brackets
  {"(" {:closing ")", :type :list},
   "[" {:closing "]", :type :vector}
   "{" {:closing "}", :type :hash-map}})

(def brackets-closing
  (->> brackets vals (map :closing) set))

(defn read-list [[token & tokens]]
  (let [{:keys [closing type]} (brackets token)]
    (loop [[token & rst :as tokens] tokens
           xs []]
      (cond
        (= closing token) [{:type type, :val xs} rst]
        (nil? token) (throw (ex-info (str "Expected '" closing "', got EOF.")
                                     {:type :parse-error}))
        (brackets-closing token) (throw (ex-info (str "Expected '" closing "', got '" token "'.")
                                                 {:type :parse-error}))
        :else (let [[x rst] (read-form tokens)]
                (recur rst (conj xs x)))))))

(defn is-symbol? [st] (re-find #"^[a-zA-Z+*/-][\w'+*/-]*$" st))
(defn is-string? [st] (re-find #"^\".*\"$" st))
(defn read-keyword [st] (some-> (re-find #"^:(.*)$" st) second))

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
         (read-keyword token) {:type :keyword, :val (-> token read-keyword ->MalKeyword)}
         :else (if-let [num (parse-num token)]
                 {:type :number, :val num}
                 (throw (ex-info (str "Failed to parse token " (pr-str token))
                                 {:type :parse-error}))))
   more])

(def special-chars
  {"'"  "quote",
   "`"  "quasiquote",
   "~"  "unquote",
   "~@" "splice-unquote"
   "@"  "deref"})

(defn read-quote [[token & more]]
  (let [[form remaining-tokens] (read-form more)]
    [{:type :list, :val [{:type :symbol :val (special-chars token)} form]}
     remaining-tokens]))

(defn read-with-meta [[token & more]]
  (let [[meta-form remaining-tokens] (read-form more)
        [target-form remaining-tokens] (read-form remaining-tokens)]
    [{:type :list, :val [{:type :symbol :val "with-meta"} target-form meta-form]}
     remaining-tokens]))

(defn read-form
  "Read a form. Returns a pair of [form remaining-tokens]"
  [[token & rst :as tokens]]
  (cond
    (nil? token) nil
    (.startsWith token ";") (recur rst)
    (brackets token) (read-list tokens)
    (special-chars token) (read-quote tokens)
    (= "^" token) (read-with-meta tokens)
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
  (-> st tokenize read-form first))
