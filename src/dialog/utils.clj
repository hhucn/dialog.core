(ns dialog.utils)

(defn map->nsmap
  "Qualify all unqualified keys in a map with a prefix."
  [m namespace]
  (reduce-kv (fn [acc k v]
               (let [new-kw (if (and (keyword? k)
                                     (not (qualified-keyword? k)))
                              (keyword (str namespace) (name k))
                              k)]
                 (assoc acc new-kw v)))
             {} m))