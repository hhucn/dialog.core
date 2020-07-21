(ns dialog.utils
  (:require [clojure.spec.alpha :as s]
            [clojure.walk :as walk])
  (:import (clojure.lang MapEntry)))

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
  :args (s/cat :m map? :namespace (s/or :k keyword? :str string? :sym symbol?))
  :ret map?
  :fn (s/and #(= (count (->> % :args :m keys (filter keyword?)))
                 (count (->> % :ret keys (filter qualified-keyword?))))))

(defn ident-map->value
  "Finds any occurrence of a member of `keys` in `coll`. Then replaced the corresponding
   value with the value of its :db/ident entry.
   E.g.
   (ident-map->value {:foo {:db/ident :bar}, :baz {:db/ident :oof}} [:foo :baz])
   => {:foo :bar, :baz :oof}

   (ident-map->value {:foo {:db/ident :bar}} [:not-found])
   => {:foo {:db/ident :bar}}"
  [coll keys]
  (walk/postwalk
    #(if (and (= MapEntry (type %)) (contains? (set keys) (first %)))
       [(first %) (:db/ident (second %))]
       %)
    coll))

(s/fdef ident-map->value
        :args (s/cat :coll map? :keys (s/coll-of keyword?)))
