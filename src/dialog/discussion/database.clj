(ns dialog.discussion.database
  [:require [dialog.discussion.models :as models]
            [dialog.discussion.config :as config]
            [dialog.discussion.test-data :as test-data]
            [dialog.utils :as utils]
            [datomic.client.api :as d]])

;; Stubs

;; TODO
(defn all-arguments-for-discussion [discussion-title]
  [])

;; TODO
(defn starting-arguments-by-title [discussion-title]
  [])

;; TODO
(defn arguments-by-title [discussion-title]
  [])

;; TODO
(defn statements-attacking-a-premise [argument]
  [])

;; TODO
(defn statements-attacking-a-conclusion [argument]
  [])

;; TODO
(defn statements-undercutting-argument [argument]
  [])

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

;; Concrete Transactions ########################
(defn save-discussion!
  "Saves discussion into the database.
  The discussion is prefixed with the discussion ns automatically for datomic."
  [discussion]
  (d/transact (new-connection)
              {:tx-data [(utils/map->nsmap discussion :discussion)]}))


(defn delete-discussion!
  "Sets the discussion with the corresponding title to `deleted`."
  [title]
  (d/transact (new-connection)
              {:tx-data [{:db/id [:discussion/title title]
                          :discussion/states #{:discussion.state/deleted}}]}))

(defn open-discussion!
  "Opens a closed discussion. Does not check whether the discussion is closed."
  [title]
  (d/transact (new-connection)
              {:tx-data [[:db/retract [:discussion/title title] :discussion/states :discussion.state/closed]
                         [:db/add [:discussion/title title] :discussion/states :discussion.state/open]]}))