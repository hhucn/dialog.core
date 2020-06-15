(ns dialog.discussion.engine)

(def database (atom {}))

(defn create-discussion
  "Returns a newly created discussion map. Checks for duplicates"
  [title description]
  {:title title
   :description description
   :created (java.time.LocalDateTime/now)
   :modified (java.time.LocalDateTime/now)
   :id (java.util.UUID/randomUUID)
   :starting-arguments []})

(defn add-starting-argument
  [discussion argument]
  "Adds a starting argument and returns the modified discussion. Checks for duplicates."
  (let [starting-arguments-ids (map :id (:starting-arguments discussion))]
    (when-not (some #(= (:id argument) %) starting-arguments-ids)
      (update discussion :starting-arguments conj argument))))

