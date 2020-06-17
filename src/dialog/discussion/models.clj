(ns dialog.discussion.models
  (:require [clojure.spec.alpha :as s]))

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

;; Specs. Please keep in sync with model above. Extend as needed.
(s/def ::title string?)
(s/def ::description string?)
(s/def ::states
  (s/coll-of #{:discussion.state/open :discussion.state/closed :discussion.state/private}
             :distinct true))
(s/def ::starting-arguments (s/coll-of map?))               ;; TODO Specify more
(s/def ::discussion (s/keys :req-un [::title ::description ::states
                                     ::starting-arguments]))