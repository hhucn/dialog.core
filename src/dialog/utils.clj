(ns dialog.utils
  (:require [clojure.spec.alpha :as s]))

(defn- keyword->str
  "Stringify a keyword without the colon."
  [k]
  (str (name k)))

(defn map->nsmap
  "Qualify all unqualified keys in a map with a prefix."
  [m namespace]
  (reduce-kv (fn [acc k v]
               (let [new-kw (if (and (keyword? k)
                                     (not (qualified-keyword? k)))
                              (keyword (keyword->str namespace) (name k))
                              k)]
                 (assoc acc new-kw v)))
             {} m))

(s/fdef
  map->nsmap
  :args (s/cat :m map? :namespace (s/or keyword? string? symbol?))
  :ret map?
  :fn (s/and #(= (count (->> % :args :m keys (filter keyword?)))
                 (count (->> % :ret keys (filter qualified-keyword?))))))