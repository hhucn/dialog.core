(ns dialog.discussion.database
  [:require [dialog.discussion.models :as models]
            [dialog.discussion.config :as config]
            [dialog.discussion.test-data :as test-data]
            [dialog.utils :as utils]
            [datomic.client.api :as d]
            [clojure.spec.alpha :as s]])

;; Setting the client to private breaks some async routine in datomic
(defonce datomic-client
  (d/client config/datomic))

(defn new-connection
  "Connects to the database and returns a connection."
  []
  (d/connect datomic-client {:db-name config/db-name}))

(defn transact
  "Shorthand for transaction."
  [data]
  (d/transact (new-connection) {:tx-data data}))

(defn- create-discussion-schema
  "Creates the schema for discussions inside the database."
  [connection]
  (d/transact connection {:tx-data models/datomic-schema}))

(defn init
  "Initialization function, which does everything needed at a fresh app-install."
  []
  (create-discussion-schema (new-connection))
  (transact test-data/testdata-cat-or-dog))

(defn- ident-map->value
  "Change an ident-map to a single value."
  [data key]
  (update data key #(:db/ident %)))


;; -----------------------------------------------------------------------------
;; Patterns

(def ^:private argument-pattern
  "Defines the default pattern for arguments. Oftentimes used in pull-patterns
  in a Datalog query bind the data to this structure."
  [:db/id
   :argument/version
   {:argument/author [:author/nickname]}
   {:argument/type [:db/ident]}
   {:argument/premises [:db/id
                        :statement/content
                        :statement/version
                        {:statement/author [:author/nickname]}]}
   {:argument/conclusion [:statement/content
                          :statement/version
                          {:statement/author [:author/nickname]}
                          :db/id]}])

(def ^:private statement-pattern
  "Representation of a statement. Oftentimes used in a Datalog pull pattern."
  [:db/id
   :statement/content
   :statement/version
   {:statement/author [:author/nickname]}])


;; -----------------------------------------------------------------------------
;; Queries

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
  "Returns all statements that are used to attack one of the premises of `argument-id`."
  [argument-id]
  (statements-attacking-part argument-id :premises))

(defn statements-attacking-conclusion
  "Returns all statements that are used to attack the conclusion of `argument-id`."
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

(defn- direct-argument-attackers
  "Queries the arguments attacking the premises or conclusion of `argument-id`."
  [argument-id qualified-attribute]
  (query-arguments
    '[:find (pull ?attacking-arguments argument-pattern)
      :in $ argument-pattern ?argument-id ?qualified-attribute
      :where [?argument-id ?qualified-attribute ?attacked-statement]
      [?attacking-arguments :argument/type :argument.type/attack]
      [?attacking-arguments :argument/conclusion ?attacked-statement]]
    argument-id qualified-attribute))

(defn arguments-attacking-premises
  "Give back all arguments that attack the premises of `argument-id`"
  [argument-id]
  (direct-argument-attackers argument-id :argument/premises))

(defn arguments-attacking-conclusion
  "Give back all arguments that attack the conclusion of `argument-id`"
  [argument-id]
  (direct-argument-attackers argument-id :argument/conclusion))

(defn undercuts-to-argument
  "Return all arguments that undercut `argument-id`."
  [argument-id]
  (query-arguments
    '[:find (pull ?undercutters argument-pattern)
      :in $ argument-pattern ?argument-id
      :where [?undercutters :argument/conclusion ?argument-id]]
    argument-id))

(defn get-attackers-for-argument
  "Returns all arguments that attack `argument-id`."
  [argument-id]
  (let [attacks-on-premises (arguments-attacking-premises argument-id)
        attacks-on-conclusion (arguments-attacking-conclusion argument-id)
        undercuts (undercuts-to-argument argument-id)]
    (concat attacks-on-conclusion attacks-on-premises undercuts)))

(defn- direct-argument-supporters
  "Queries the arguments attacking the premises or conclusion of `argument-id`."
  [argument-id qualified-attribute]
  (query-arguments
    '[:find (pull ?supporting-arguments argument-pattern)
      :in $ argument-pattern ?argument-id ?qualified-attribute
      :where [?argument-id ?qualified-attribute ?supported-statement]
      [?supporting-arguments :argument/type :argument.type/support]
      [?supporting-arguments :argument/conclusion ?supported-statement]]
    argument-id qualified-attribute))

(defn arguments-supporting-premises
  "All arguments that support the premises of `argument-id`."
  [argument-id]
  (direct-argument-supporters argument-id :argument/premises))

(defn arguments-supporting-conclusion
  "All arguments that support the conclusion of `argument-id`."
  [argument-id]
  (direct-argument-supporters argument-id :argument/conclusion))

(defn support-for-argument
  "Returns all arguments supporting the premises or conclusion of `argument-id`."
  [argument-id]
  (concat (arguments-supporting-premises argument-id)
          (arguments-supporting-conclusion argument-id)))

(comment
  (support-for-argument 17592186045447)
  (count (starting-arguments-by-title "Cat or Dog?"))
  (count (all-arguments-for-discussion "Cat or Dog?"))
  :end)


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


;; -----------------------------------------------------------------------------
;; Query entities

(defn discussion-id-by-title
  "Look in the database and return the discussion id by its title."
  [title]
  (ffirst (d/q '[:find ?e
                 :in $ ?title
                 :where [?e :discussion/title ?title]]
               (d/db (new-connection)), title)))

(s/fdef discussion-id-by-title
        :args (s/cat :title string?)
        :ret (s/? number?))

(defn all-discussion-titles-and-ids []
  (d/q '[:find ?e ?title
         :in $
         :where [?e :discussion/title ?title]]
       (d/db (new-connection))))

(s/fdef all-discussion-titles-and-ids
        :ret (s/? number?))


;; -----------------------------------------------------------------------------
;; Write new discussion entities

(defn new-argument!
  "Creates a new argument and stores it in the database."
  [discussion-title author-nickname conclusion & premises]
  (transact
    [(let [query-author [:author/nickname author-nickname]]
       {:argument/author query-author
        :argument/premises (mapv (fn [premise] {:db/id premise
                                                :statement/author query-author
                                                :statement/content premise
                                                :statement/version 1})
                                 premises)
        :argument/conclusion {:db/id conclusion
                              :statement/author query-author
                              :statement/content conclusion
                              :statement/version 1}
        :argument/version 1
        :argument/type :argument.type/support
        :argument/discussions [(discussion-id-by-title discussion-title)]})]))
(s/fdef new-argument!
        :args (s/cat :discussion-title string?
                     :author-nickname string?
                     :conclusion string?
                     :premises (s/* string?))
        :ret map?)

(comment
  (discussion-id-by-title "Cat or Dog?")
  (new-argument! "Cat or Dog?" "Christian" "this is sparta" "foo" "bar" "baz")
  (d/q '[:find ?e
         :in $ ?title
         :where [?e :statement/content ?title]]
       (d/db (new-connection)), "foo")
  :end)