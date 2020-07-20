(ns dialog.discussion.database
  [:require [dialog.discussion.models :as models]
            [dialog.discussion.test-data :as test-data]
            [dialog.utils :as utils]
            [datomic.client.api :as d]
            [clojure.spec.alpha :as s]
            [clojure.walk :as walk]]
  (:import (clojure.lang MapEntry)))

(def db-config (atom {}))

(defn new-connection
  "Connects to the database and returns a connection."
  []
  (d/connect (d/client (:datomic @db-config)) {:db-name (:name @db-config)}))

(defn transact
  "Shorthand for transaction."
  [data]
  (d/transact (new-connection) {:tx-data data}))

(defn- create-discussion-schema
  "Creates the schema for discussions inside the database."
  [connection]
  (d/transact connection {:tx-data models/datomic-schema}))

(defn init!
  "Initialization function, which does everything needed at a fresh start.
  `config` must be a map which at lease contains `:datomic` with the datomic
  config as a value and `:db-name` with the database name as a value."
  [config]
  (reset! db-config config)
  (create-discussion-schema (new-connection)))

(defn change-config!
  "Swap out the config for the database."
  [new-config]
  (reset! db-config new-config))

(defn load-testdata
  "Load the toy example 'Cat or Dog?' discussion if needed."
  []
  (transact test-data/testdata-cat-or-dog))

(defn- ident-map->value
  "Finds any occurrence of a member of `keys` in `coll`. Then replaced the corresponding
   value with the value of its :db/ident entry.
   E.g.
   (ident-map->value {:foo {:db/ident :bar}, :baz {:db/ident :oof}} [:foo :baz])
   => {:foo :bar, :baz :oof}

   (ident-map->value {:foo {:db/ident :bar}} [:not-found])
   => {:foo {:db/ident :bar}}"
  [coll keys]
  (walk/postwalk
    #(if (and (= MapEntry (type %)) (contains? (set keys) (first %)))
       [(first %) (:db/ident (second %))]
       %)
    coll))

;; -----------------------------------------------------------------------------
;; Patterns

(def ^:private statement-pattern
  "Representation of a statement. Oftentimes used in a Datalog pull pattern."
  [:db/id
   :statement/content
   :statement/version
   {:statement/author [:author/nickname]}])

(def ^:private argument-pattern
  "Defines the default pattern for arguments. Oftentimes used in pull-patterns
  in a Datalog query bind the data to this structure."
  [:db/id
   :argument/version
   {:argument/author [:author/nickname]}
   {:argument/type [:db/ident]}
   {:argument/premises statement-pattern}
   {:argument/conclusion
    (conj statement-pattern
          :argument/version
          {:argument/author [:author/nickname]}
          {:argument/type [:db/ident]}
          {:argument/premises [:db/id
                               :statement/content
                               :statement/version
                               {:statement/author [:author/nickname]}]}
          {:argument/conclusion statement-pattern})}])


;; -----------------------------------------------------------------------------
;; Queries

(defn- query-arguments
  "Takes a `query` that returns arguments and applies an `argument-pattern` to it as
  a second argument. Optional arguments for the query are input as third, fourth, etc.
  parameter. A prettified list is returned."
  [query & args]
  (let [db (d/db (new-connection))
        arguments (apply d/q query db argument-pattern args)]
    (map #(ident-map->value (first %) [:argument/type]) arguments)))

(defn all-arguments-for-discussion
  "Returns all arguments belonging to a discussion, identified by discussion id."
  [discussion-id]
  (query-arguments
    '[:find (pull ?discussion-arguments argument-pattern)
      :in $ argument-pattern ?discussion-id
      :where [?discussion-arguments :argument/discussions ?discussion-id]]
    discussion-id))

(defn all-arguments-for-conclusion
  "Get all arguments for a given conclusion."
  [conclusion]
  (query-arguments
    '[:find (pull ?arguments argument-pattern)
      :in $ argument-pattern ?conclusion
      :where [?arguments :argument/conclusion ?conclusion]]
    conclusion))

(s/fdef all-arguments-for-conclusion
        :args (s/cat :conclusion number?))

(defn starting-arguments-by-discussion
  "Deep-Query all starting-arguments of a certain discussion."
  [discussion-id]
  (query-arguments
    '[:find (pull ?starting-arguments argument-pattern)
      :in $ argument-pattern ?discussion-id
      :where [?discussion-id :discussion/starting-arguments ?starting-arguments]]
    discussion-id))

(defn starting-conclusions-by-discussion
  "Get all statements / conclusions (formerly positions) from the starting
  arguments. Return only all non-undercuts."
  [discussion-id]
  (let [starting-arguments (starting-arguments-by-discussion discussion-id)
        possible-statements (map :argument/conclusion starting-arguments)]
    (filter #(s/valid? ::models/statement %) possible-statements)))

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
  "Queries the arguments supporting the premises or conclusion of `argument-id`."
  [argument-id qualified-attribute]
  (query-arguments
    '[:find (pull ?supporting-arguments argument-pattern)
      :in $ argument-pattern ?argument-id ?qualified-attribute
      :where [?argument-id ?qualified-attribute ?supported-statement]
      [?supporting-arguments :argument/type :argument.type/support]
      [?supporting-arguments :argument/conclusion ?supported-statement]]
    argument-id qualified-attribute))

(s/fdef direct-argument-supporters
        :args (s/cat :argument-id number? :qualified-attribute keyword?)
        :ret (s/coll-of ::models/argument))

(defn arguments-supporting-premises
  "All arguments that support the premises of `argument-id`."
  [argument-id]
  (direct-argument-supporters argument-id :argument/premises))

(s/fdef arguments-supporting-premises
        :args (s/cat :argument-id number?)
        :ret (s/coll-of ::models/argument))

(defn arguments-supporting-conclusion
  "All arguments that support the conclusion of `argument-id`."
  [argument-id]
  (direct-argument-supporters argument-id :argument/conclusion))

(s/fdef arguments-supporting-conclusion
        :args (s/cat :argument-id number?)
        :ret (s/coll-of ::models/argument))

(defn support-for-argument
  "Returns all arguments supporting the premises or conclusion of `argument-id`."
  [argument-id]
  (concat (arguments-supporting-premises argument-id)
          (arguments-supporting-conclusion argument-id)))

(s/fdef support-for-argument
        :args (s/cat :argument-id number?)
        :ret (s/coll-of ::models/argument))

(comment
  (support-for-argument 17592186045447)
  (count (starting-arguments-by-discussion 17592186045477))
  (count (all-arguments-for-discussion 17592186045477))
  :end)


;; Concrete Transactions ########################
(defn save-discussion!
  "Saves discussion into the database.
  The discussion is prefixed with the discussion ns automatically for datomic."
  [discussion]
  (transact [(utils/map->nsmap discussion :discussion)]))


(defn delete-discussion!
  "Sets the discussion with the corresponding discussion to `deleted`."
  [discussion-id]
  (transact [{:db/id discussion-id
              :discussion/states #{:discussion.state/deleted}}]))

(defn reopen-discussion!
  "Opens a closed discussion. Does not check whether the discussion is closed."
  [discussion-id]
  (transact [[:db/retract discussion-id :discussion/states :discussion.state/closed]
             [:db/add discussion-id :discussion/states :discussion.state/open]]))

(defn close-discussion!
  "Close a discussion."
  [discussion-id]
  (transact [[:db/retract discussion-id :discussion/states :discussion.state/open]
             [:db/add discussion-id :discussion/states :discussion.state/closed]]))


;; -----------------------------------------------------------------------------
;; Query entities

(defn all-discussion-titles-and-ids
  "Query the database for some information about discussions."
  []
  (d/q '[:find ?e ?title
         :in $
         :where [?e :discussion/title ?title]]
       (d/db (new-connection))))

(s/fdef all-discussion-titles-and-ids
        :ret (s/? number?))

(defn all-arguments-by-content
  "Query database for exact content matches of a statement and return the
  corresponding arguments."
  [content]
  (vec (set
         (query-arguments
           '[:find (pull ?statements-in-premise argument-pattern) (pull ?statements-in-conclusion argument-pattern)
             :in $ argument-pattern ?content
             :where [?statements :statement/content ?content]
             [?statements-in-premise :argument/premises ?statements]
             [?statements-in-conclusion :argument/conclusion ?statements]]
           content))))

(s/fdef all-arguments-by-content
        :args (s/cat :content :statement/content)
        :ret (s/coll-of ::models/argument))


;; -----------------------------------------------------------------------------
;; Write new discussion entities

(defn- pack-premises
  "Packs premises into a statement-structure."
  [premises author-nickname]
  (mapv (fn [premise] {:db/id premise
                       :statement/author [:author/nickname author-nickname]
                       :statement/content premise
                       :statement/version 1})
        premises))

(defn- prepare-new-argument
  "Prepares a new argument for transaction. Optionally sets a temporary id."
  ([discussion-id author-nickname conclusion premises temporary-id]
   (merge
     (prepare-new-argument discussion-id author-nickname conclusion premises)
     {:db/id temporary-id}))
  ([discussion-id author-nickname conclusion premises]
   {:argument/author [:author/nickname author-nickname]
    :argument/premises (pack-premises premises author-nickname)
    :argument/conclusion {:db/id conclusion
                          :statement/author [:author/nickname author-nickname]
                          :statement/content conclusion
                          :statement/version 1}
    :argument/version 1
    :argument/type :argument.type/support
    :argument/discussions [discussion-id]}))

(s/fdef prepare-new-argument
        :args (s/cat :discussion-title number?
                     :author-nickname string?
                     :conclusion string?
                     :premises (s/coll-of string?)
                     :temporary-id (s/? string?))
        :ret map?)

(defn- new-premises-for-argument!
  "Creates a new argument based on the old argument, but adding new premises and
  a new author. The old premise(s) now become(s) the new conclusion(s). Takes an
  argument type to represent a generic argument construction function."
  [discussion-id author-nickname argument premises argument-type]
  (let [premise-ids (map :db/id (:argument/premises argument))
        new-arguments (for [premise-id premise-ids]
                        {:argument/author [:author/nickname author-nickname]
                         :argument/premises (pack-premises premises author-nickname)
                         :argument/conclusion premise-id
                         :argument/version 1
                         :argument/type argument-type
                         :argument/discussions [discussion-id]})]
    (transact new-arguments)))

(s/fdef new-premises-for-argument!
        :args (s/cat :discussion-id number? :author-nickname :author/nickname
                     :argument ::models/argument :premises (s/coll-of string?)
                     :argument-type :argument/type))

(defn- prepare-argument-with-conclusion-reference
  "Creates new argument, but references the old conclusion by id."
  [discussion-id author-nickname conclusion-id premises argument-type]
  {:argument/author [:author/nickname author-nickname]
   :argument/premises (pack-premises premises author-nickname)
   :argument/conclusion conclusion-id
   :argument/version 1
   :argument/type argument-type
   :argument/discussions [discussion-id]})

(s/fdef prepare-argument-with-conclusion-reference
        :args (s/cat :discussion-id number?
                     :author/nickname :author/nickname
                     :conclusion-id number?
                     :premises (s/coll-of string?)
                     :argument-type :argument/type))

(defn support-argument!
  "Adds new statements support the argument's premises."
  [discussion-id author-nickname argument premises]
  (new-premises-for-argument! discussion-id author-nickname argument premises :argument.type/support))

(s/fdef support-argument!
        :args (s/cat :discussion-id number? :author-nickname :author/nickname
                     :argument (s/keys :req [:argument/premises])
                     :premises (s/coll-of string?)))

(defn undermine-argument!
  "Attack the argument's premises with own statements."
  [discussion-id author-nickname argument premises]
  (new-premises-for-argument! discussion-id author-nickname argument premises :argument.type/attack))

(s/fdef undermine-argument!
        :args (s/cat :discussion-id number? :author-nickname :author/nickname
                     :argument (s/keys :req [:argument/premises])
                     :premises (s/coll-of string?)))

(defn rebut-argument!
  "Attack the argument's conclusion with own statements."
  [discussion-id author-nickname argument premises]
  (let [conclusion-id (get-in argument [:argument/conclusion :db/id])]
    (transact
      [(prepare-argument-with-conclusion-reference
         discussion-id author-nickname conclusion-id
         premises :argument.type/attack)])))

(s/fdef rebut-argument!
        :args (s/cat :discussion-id number? :author-nickname :author/nickname
                     :argument (s/keys :req [:argument/conclusion])
                     :premises (s/coll-of string?)))

(defn defend-argument!
  "Support the argument's conclusion with own premises"
  [discussion-id author-nickname argument premises]
  (let [conclusion-id (get-in argument [:argument/conclusion :db/id])]
    (transact
      [(prepare-new-argument
         discussion-id author-nickname conclusion-id
         premises :argument.type/support)])))

(s/fdef defend-argument!
        :args (s/cat :discussion-id number? :author-nickname :author/nickname
                     :argument (s/keys :req [:argument/conclusion])
                     :premises (s/coll-of string?)))

(defn undercut-argument!
  "Undercut an argument and store it to the database."
  [discussion-id author-nickname {:keys [db/id]} premises]
  (transact
    [{:argument/author [:author/nickname author-nickname]
      :argument/premises (pack-premises premises author-nickname)
      :argument/conclusion id
      :argument/version 1
      :argument/type :argument.type/undercut
      :argument/discussions [discussion-id]}]))

(s/fdef undercut-argument!
        :args (s/cat :discussion-id number? :author-nickname :author/nickname
                     :argument (s/keys :req [:db/id])
                     :premises (s/coll-of string?)))

(defn add-new-starting-argument!
  "Creates a new starting argument in a discussion."
  [discussion-id author-nickname conclusion premises]
  (let [new-argument (prepare-new-argument discussion-id author-nickname conclusion premises "add/starting-argument")
        temporary-id (:db/id new-argument)]
    (transact [new-argument
               [:db/add discussion-id :discussion/starting-arguments temporary-id]])))

(comment
  (all-arguments-by-content "we should get a dog")
  (add-new-starting-argument! 17592186045477 "Christian" "this is sparta" ["foo" "bar" "baz"])
  (all-arguments-for-discussion 17592186045477)

  (declare testargument)
  (undermine-argument! 17592186045477 "Christian" testargument ["irgendwas zum underminen"])
  (rebut-argument! 17592186045477 "Christian" testargument ["das ist eine doofe idee" "weil isso"])

  (def testargument
    {:db/id 17592186045475,
     :argument/version 1,
     :argument/author #:author{:nickname "Christian"},
     :argument/type :argument.type/support,
     :argument/premises [{:db/id 17592186045476,
                          :statement/content "several cats of my friends are real assholes",
                          :statement/version 1,
                          :statement/author #:author{:nickname "Christian"}}],
     :argument/conclusion {:db/id 17592186045468,
                           :statement/content "cats are capricious",
                           :statement/version 1,
                           :statement/author #:author{:nickname "Wegi"}}})
  :end)