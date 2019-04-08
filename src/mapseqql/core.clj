(ns mapseqql.core
  (:require
   [clojure.string :refer [trim]]
   [clojure.java.io :as io]
   ;[clojure.walk :refer [postwalk]]
   [instaparse.core :as insta]))

(def mini-sql-grammar
  (->> "mini-sql-grammar.ebnf"
       io/resource
       slurp))

(def mini-sql-parser
  (insta/parser mini-sql-grammar))

(defmulti parse first)
(defmethod parse :string  [[_ s]] (trim s))
(defmethod parse :keyword [[_ k]] (keyword (subs (trim k) 1)))
(defmethod parse :col-name [[_ n]] (parse n))
(defmethod parse \, [& _] nil)
(defmethod parse :star [& _] ::all)

(defn- select-columns
  [cols m]
  ;(clojure.pprint/pprint [:cols cols])
  (case cols
    [::all] m
    (select-keys m cols)))

(defn- apply-query
  [[_query [_select _ & columns] [_from _ [_ table-name]]] data]
  (let [cols   (remove nil? (map parse columns))
        mapseq (get data (parse table-name))]
    (map (partial select-columns cols) mapseq)))

(defn execute
  [query data]
  (let [q (mini-sql-parser query)]
    ;(clojure.pprint/pprint [:q q])
    (apply-query q data)))

(comment
  (def example-dataset [{:character "Goku" :ki-level 10}
                        {:character "Goku (Great Ape)" :ki-level 100}
                        {:character "Emperor Pilaf" :ki-level 40}
                        {:character "Shu" :ki-level 20}
                        {:character "Mai" :ki-level 20}
                        {:character "Oolong" :ki-level 10}
                        {:character "Puar" :ki-level 10}
                        {:character "Ox-King" :ki-level 900}])

  (prn mini-sql-parser)

  (execute "select * from ds;" {"ds" example-dataset})
  (execute "select :ki-level, :character from ds;" {"ds" example-dataset}))
