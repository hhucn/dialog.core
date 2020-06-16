(ns dialog.discussion.database
  [:require [dialog.core.models :as models]
            [dialog.discussion.config :as config]
            [datomic.client.api :as d]])

;; Setting the client to private breaks some async routine in datomic
(defonce datomic-client (d/client config/datomic))

(def connection
  (d/connect datomic-client {:db-name config/db-name}))

(defn- create-discussion-schema
  "Creates the schema for discussions inside the database"
  []
  (d/transact connection {:tx-data models/datomic-schema}))


(defn init
  "Initialization function, which does everything needed at a fresh app-install."
  []
  (create-discussion-schema))