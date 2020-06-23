(ns dialog.discussion.database
  [:require [dialog.discussion.models :as models]
            [dialog.discussion.config :as config]
            [dialog.discussion.test-data :as test-data]
            [dialog.utils :as utils]
            [datomic.client.api :as d]])

;; Setting the client to private breaks some async routine in datomic
(defonce datomic-client (d/client config/datomic))

(defn new-connection
  []
  (d/connect datomic-client {:db-name config/db-name}))

(defn transact
  "Shorthand for transaction"
  [data]
  (d/transact (new-connection) {:tx-data data}))

(defn- create-discussion-schema
  "Creates the schema for discussions inside the database"
  [connection]
  (d/transact connection {:tx-data models/datomic-schema}))

(defn init
  "Initialization function, which does everything needed at a fresh app-install."
  []
  (create-discussion-schema (new-connection))
  (transact test-data/testdata-cat-or-dog))

(def ^:private argument-pattern
  [:argument/version
   {:argument/author [:author/nickname]}
   {:argument/type [:db/ident]}
   {:argument/premises [:statement/content
                        :statement/version
                        {:statement/author [:author/nickname]}]}
   {:argument/conclusion [:statement/content
                          :statement/version
                          {:statement/author [:author/nickname]}]}])

(defn- ident-map->value
  "Change an ident-map to a single value"
  [data key]
  (update data key #(:db/ident %)))

(defn- query-arguments
  "Takes a `query` that returns arguments and applies an `argument-pattern` to it as
  a second argument. Optional arguments for the query are input as third, fourth, etc.
  parameter. A prettified list is returned."
  [query & args]
  (let [db (d/db (new-connection))
        arguments (apply d/q query db argument-pattern args)]
    (map #(ident-map->value (first %) :argument/type) arguments)))

(defn all-arguments-for-discussion
  "Returns all arguments belonging to a discussion, identified by title."
  [discussion-title]
  (query-arguments
    '[:find (pull ?discussion-arguments argument-pattern)
      :in $ argument-pattern ?discussion-title
      :where [?discussion :discussion/title ?discussion-title]
      [?discussion-arguments :argument/discussions ?discussion]]
    discussion-title))

(defn starting-arguments-by-title
  "Deep-Query all starting-arguments of a certain discussion."
  [discussion-title]
  (query-arguments
    '[:find (pull ?starting-arguments argument-pattern)
      :in $ argument-pattern ?discussion-title
      :where [?discussion :discussion/title ?discussion-title]
      [?discussion :discussion/starting-arguments ?starting-arguments]]
    discussion-title))

(comment
  (count (starting-arguments-by-title "Cat or Dog?"))
  (count (all-arguments-for-discussion "Cat or Dog?"))
  )


;; Concrete Transactions ########################
(defn save-discussion!
  "Saves discussion into the database.
  The discussion is prefixed with the discussion ns automatically for datomic."
  [discussion]
  (transact [(utils/map->nsmap discussion :discussion)]))


(defn delete-discussion!
  "Sets the discussion with the corresponding title to `deleted`."
  [title]
  (transact [{:db/id [:discussion/title title]
              :discussion/states #{:discussion.state/deleted}}]))

(defn reopen-discussion!
  "Opens a closed discussion. Does not check whether the discussion is closed."
  [title]
  (transact [[:db/retract [:discussion/title title] :discussion/states :discussion.state/closed]
             [:db/add [:discussion/title title] :discussion/states :discussion.state/open]]))