(ns dialog.core.models
  (:require [hodur-engine.core :as hodur]
            [hodur-spec-schema.core :as hodur-spec]
            [hodur-datomic-schema.core :as hodur-datomic]))

(def datomic-schema
  [;; Statement
   {:db/ident :statement/author
    :db/valueType :db.type/ref
    :db/cardinality :db.cardinality/one
    :db/doc "The author of the statement"}
   {:db/ident :statement/content
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one
    :db/doc "The text-content of the statement"}
   {:db/ident :statement/version
    :db/valueType :db.type/long
    :db/cardinality :db.cardinality/one
    :db/doc "The version of the statement. Always positive"}
   ;; Author
   {:db/ident :author/nickname
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one
    :db/doc "The nickname of an author"}
   ;; Argument Types
   {:db/ident :argument.type/support}
   {:db/ident :argument.type/attack}
   {:db/ident :argument.type/undercut}
   ;; Argument
   {:db/ident :argument/author
    :db/valueType :db.type/ref
    :db/cardinality :db.cardinality/one
    :db/doc "The author of an argument"}
   {:db/ident :argument/premises
    :db/valueType :db.type/ref
    :db/cardinality :db.cardinality/many
    :db/doc "The premises of an argument constituting a premise-group."}
   {:db/ident :argument/type
    :db/valueType :db.type/ref
    :db/cardinality :db.cardinality/one
    :db/doc "The type of the arguments edge"}
   {:db/ident :argument/conclusion
    :db/valueType :db.type/ref
    :db/cardinality :db.cardinality/one
    :db/doc "The conclusion of an argument"}
   {:db/ident :argument/version
    :db/valueType :db.type/long
    :db/cardinality :db.cardinality/one
    :db/doc "The version of an argument"}
   ;; Discussion States
   {:db/ident :discussion.state/open}
   {:db/ident :discussion.state/closed}
   {:db/ident :discussion.state/private}
   ;; Discussion
   {:db/ident :discussion/title
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one
    :db/doc "The title / heading of a discussion"}
   {:db/ident :discussion/description
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one
    :db/doc "The topic description of a discussion"}
   {:db/ident :discussion/states
    :db/valueType :db.type/ref
    :db/cardinality :db.cardinality/many
    :db/doc "The states the discussion is in"}
   {:db/ident :discussion/starting-arguments
    :db/valueType :db.type/ref
    :db/cardinality :db.cardinality/many
    :db/doc "The arguments at the source of the discussion-graph"}])

(def core-data
  (hodur/init-schema
    '[^{:spec/tag true
        :datomic/tag true}
      default

      Statement
      [^Author author
       ^String content
       ^{:type Integer
         :default 1} version
       ^DateTime created
       ^DateTime modified]

      Author
      [^String nickname
       ^DateTime created
       ^DateTime modified]

      ^{:union true}
      ArgumentTarget
      [Statement Argument]

      ^{:enum true}
      ArgumentType
      [support attack undercut]

      Argument
      [^Author author
       ^{:type Statement
         :cardinality [0 n]} premises
       ^ArgumentType type
       ^ArgumentTarget conclusion
       ^DateTime created
       ^DateTime modified
       ^{:type Integer
         :default 1} version]

      ^{:enum true}
      State
      [closed private open]

      Discussion
      [^String title
       ^String description
       ^DateTime created
       ^DateTime modified
       ^{:type Argument
         :cardinality [0 n]} starting-arguments
       ^{:type State
         :cardinality [0 n]} state]]))

(def spec-schema (hodur-spec/schema core-data))
;; This defspecs needs to be run to have the specs in this namespace.
(hodur-spec/defspecs core-data)
(comment spec-schema)
;; The following will define all specs with the prefix foo instead of this namespace.
;; We can use this in tests to generate correct specs. There seems to be no option to cut the namespace
;; alltogether. one could annotate aliases for every attribute, but that is as tedious as writing
;; separate specs and datomic.
(comment (hodur-spec/defspecs core-data {:prefix :foo}))

(def datomic-schema (hodur-datomic/schema core-data))
(comment datomic-schema)