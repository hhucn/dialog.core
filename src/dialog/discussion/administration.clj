(ns dialog.discussion.administration
  (:require [clojure.spec.alpha :as s]
            [dialog.core.models :as models]))

(def database (atom {:closed-discussions #{}}))
;; :closed-discussions must be a set that is present. Otherwise the close and
;; open discussions functions behave improperly. Wont matter once, we use a real
;; database

;; TODO remove pre and post for fdef or similar
(defn empty-discussion
  "Returns a newly created discussion map."
  ([title description]
   (empty-discussion title description {}))
  ([title description opts]
   {:pre [(s/valid? :dialog.core.models.discussion/title title)
          (s/valid? :dialog.core.models.discussion/description description)]
    :post [(s/valid? ::models/discussion %)]
    }
   (merge
     {:states [:discussion.state/open]
      :starting-arguments []}
     opts
     {:title title
      :description description})))

;; TODO this has to be rewritten to work with a real db
(defn add-starting-argument
  "Adds a starting argument and returns the modified discussion. Checks for duplicates."
  [discussion argument]
  (let [starting-arguments-ids
        (map :discussion/id (:discussion/starting-arguments discussion))]
    (when-not (some #(= (:discussion/id argument) %) starting-arguments-ids)
      (update discussion :discussion/starting-arguments conj argument))))
