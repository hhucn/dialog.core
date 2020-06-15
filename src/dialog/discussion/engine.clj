(ns dialog.discussion.engine)

(def database (atom {:closed-discussions #{}}))
;; :closed-discussions must be a set that is present. Otherwise the close and
;; open discussions functions behave improperly. Wont matter once, we use a real
;; database

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

(defn save-discussion!
  "Saves discussion into the database."
  [discussion]
  (swap! database assoc-in [:discussions (:id discussion)] discussion))

(defn delete-discussion!
  "Deletes the discussion from the database."
  [discussion]
  (swap! database update :discussions dissoc (:id discussion)))

(defn close-discussion!
  "Closes a discussion. Users can not discuss in this issue anymore."
  [discussion]
  (swap! database update :closed-discussions conj (:id discussion)))

(defn open-discussion!
  "Opens a closed discussion. Does not check whether the discussion is closed."
  [discussion]
  (swap! database update :closed-discussions disj (:id discussion)))
