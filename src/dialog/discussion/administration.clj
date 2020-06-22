(ns dialog.discussion.administration
  (:require [clojure.spec.alpha :as s]
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
