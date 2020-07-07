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

(defn delete-discussion!
  "Marks a discussion as deleted. Users are not seeing the discussion anymore."
  [discussion-title]
  (database/delete-discussion! discussion-title))

(defn close-discussion!
  "Marks a discussion as closed. Users are able to see the discussion, but can not
  contribute to it anymore."
  [discussion-title]
  (database/close-discussion! discussion-title))

(defn reopen-discussion!
  "Reopens an already closed discussion."
  [discussion-title]
  (database/reopen-discussion! discussion-title))