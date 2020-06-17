(ns dialog.discussion.administration
  (:import (java.util Date))
  (:require [clojure.spec.alpha :as s]
            [dialog.core.models :as models]))

(def database (atom {:closed-discussions #{}}))
;; :closed-discussions must be a set that is present. Otherwise the close and
;; open discussions functions behave improperly. Wont matter once, we use a real
;; database

;; TODO move this to a util namespace
(defn- map->nsmap
  [m n]
  (reduce-kv (fn [acc k v]
               (let [new-kw (if (and (keyword? k)
                                     (not (qualified-keyword? k)))
                              (keyword (str n) (name k))
                              k)]
                 (assoc acc new-kw v)))
             {} m))

(defn empty-discussion
  "Returns a newly created discussion map."
  [title description & opts]
  {:pre [(s/valid? :dialog.core.models.discussion/title title)
         (s/valid? :dialog.core.models.discussion/description description)]
   :post [(s/valid? ::models/discussion %)]}
  (merge
    {:created (Date.)
     :modified (Date.)
     :state [:open]
     :starting-arguments []}
    opts
    {:title title
     :description description}))

;; TODO this has to be rewritten to work with a real db
(defn add-starting-argument
  "Adds a starting argument and returns the modified discussion. Checks for duplicates."
  [discussion argument]
  (let [starting-arguments-ids
        (map :discussion/id (:discussion/starting-arguments discussion))]
    (when-not (some #(= (:discussion/id argument) %) starting-arguments-ids)
      (update discussion :discussion/starting-arguments conj argument))))

;; TODO this has to be rewritten to work with a real db
(defn- save-discussion!
  "Saves discussion into the database."
  [discussion]
  (swap! database assoc-in [:discussions (:id discussion)] discussion))

;; TODO this has to be rewritten to work with a real db
(defn delete-discussion!
  "Deletes the discussion from the database."
  [discussion]
  (swap! database update :discussions dissoc (:id discussion)))

;; TODO this has to be rewritten to work with a real db
(defn close-discussion!
  "Closes a discussion. Users can not discuss in this issue anymore."
  [discussion]
  (swap! database update :closed-discussions conj (:id discussion)))

;; TODO this has to be rewritten to work with a real db
(defn open-discussion!
  "Opens a closed discussion. Does not check whether the discussion is closed."
  [discussion]
  (swap! database update :closed-discussions disj (:id discussion)))
