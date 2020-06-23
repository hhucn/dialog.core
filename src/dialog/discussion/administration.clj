(ns dialog.discussion.administration
  (:require [clojure.spec.alpha :as s]
            [dialog.discussion.database :as database]
            [dialog.discussion.models :as models]))

(defn empty-discussion
  "Returns a newly created discussion map."
  ([title description]
   (empty-discussion title description {}))
  ([title description opts]
   (merge
     {:discussion/states [:discussion.state/open]
      :discussion/starting-arguments []}
     opts
     {:discussion/title title
      :discussion/description description})))

(s/fdef empty-discussion
        :args (s/cat :title string? :description string?)
        :ret ::models/discussion)

(defn open-discussion!
  "Create a new discussion in the database. Returns the discussion if successful,
  or nil when the title is already reserved."
  [title description]
  (try
    (let [new-discussion (empty-discussion title description)]
      (database/save-discussion! new-discussion)
      new-discussion)
    (catch Exception _e
      nil)))
