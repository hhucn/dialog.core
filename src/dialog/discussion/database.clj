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
  [:db/id
   :argument/version
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

(def ^:private statement-pattern
  [:db/id
   :statement/content
   :statement/version
   {:statement/author [:author/nickname]}])

(defn- statements-attacking-part
  "Generic template query for statements either attacking a conclusion or the premises
  of an argument."
  [argument-id conclusion-or-premises]
  (let [db (d/db (new-connection))
        part-attribute (if (= :conclusion conclusion-or-premises)
                         :argument/conclusion
                         :argument/premises)]
    (d/q
      '[:find (pull ?attacking-premises statement-pattern)
        :in $ statement-pattern ?argument-id ?part-attribute
        :where [?argument-id ?part-attribute ?part]
        ;; Give me the arguments where our premise is the conclusion and the type is
        ;; an attack
        [?potential-attackers :argument/conclusion ?part]
        [?potential-attackers :argument/type :argument.type/attack]
        [?potential-attackers :argument/premises ?attacking-premises]]
      db statement-pattern argument-id part-attribute)))

(defn statements-attacking-premise
  "Returns all statements that are used to attack one of the premises of `argument-id`"
  [argument-id]
  (statements-attacking-part argument-id :premises))

(defn statements-attacking-conclusion
  "Returns all statements that are used to attack the conclusion of `argument-id`"
  [argument-id]
  (statements-attacking-part argument-id :conclusion))

(defn statements-undercutting-argument
  "Returns all statements uses to undercut `argument`."
  [argument-id]
  (let [db (d/db (new-connection))]
    (d/q
      '[:find (pull ?undercutting-premises statement-pattern)
        :in $ statement-pattern ?argument-id
        :where [?undercutting-arguments :argument/conclusion ?argument-id]
        [?undercutting-arguments :argument/premises ?undercutting-premises]]
      db statement-pattern argument-id)))

;; TODO does only return first pull syntax for some reason
;; TODO according to docs it should pull all three...
(defn get-attackers-for-argument
  "Returns all arguments that attack `argument-id`"
  [argument-id]
  (query-arguments
    '[:find (pull ?undercuts argument-pattern)
      (pull ?attacking-premises argument-pattern)
      (pull ?attacking-conclusions argument-pattern)
      :in $ argument-pattern ?argument-id
      :where [?undercuts :argument/conclusion ?argument-id]
      [?argument-id :argument/premises ?attacked-premises]
      [?attacking-premises :argument/conclusion ?attacked-premises]
      [?attacking-premises :argument/type :argument.type/attack]
      [?argument-id :argument/conclusion ?attacked-conclusion]
      [?attacking-conclusions :argument/conclusion ?attacked-conclusion]
      [?attacking-conclusions :argument/type :argument.type/attack]]
    argument-id))
(get-attackers-for-argument 17592186045447)
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

(defn close-discussion!
  "Close a discussion."
  [title]
  (transact [[:db/retract [:discussion/title title] :discussion/states :discussion.state/open]
             [:db/add [:discussion/title title] :discussion/states :discussion.state/closed]]))