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
    :db/unique :db.unique/value
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
   {:db/ident :argument/discussions
    :db/valueType :db.type/ref
    :db/cardinality :db.cardinality/many
    :db/doc "The discussions in which the argument is used"}
   ;; Discussion States
   {:db/ident :discussion.state/open}
   {:db/ident :discussion.state/closed}
   {:db/ident :discussion.state/private}
   {:db/ident :discussion.state/deleted}
   ;; Deletion is a marker. We don't really delete anything from datomic
   ;; Discussion
   {:db/ident :discussion/title
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one
    :db/doc "The title / heading of a discussion. This should be system-widely unique."}
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

;; -----------------------------------------------------------------------------
;; Specs. Please keep in sync with model above. Extend as needed.

;; General
(s/def :db/id (s/or :transacted number? :temp string?))

;; Discussion
(s/def :discussion/title string?)
(s/def :discussion/description string?)
(s/def :discussion/states
  (s/coll-of #{:discussion.state/open :discussion.state/closed
               :discussion.state/private :discussion.state/deleted}
             :distinct true))
(s/def :discussion/starting-arguments (s/coll-of ::argument))
(s/def ::discussion (s/keys :req [:discussion/title :discussion/description
                                  :discussion/states :discussion/starting-arguments]))

;; Author
(s/def :author/nickname string?)
(s/def ::author (s/keys :req [:author/nickname]))

;; Statement
(s/def :statement/content string?)
(s/def :statement/version number?)
(s/def :statement/author ::author)
(s/def ::statement
  (s/keys :req [:statement/content :statement/version :statement/author]))

;; Argument
(s/def :argument/type
  #{:argument.type/attack :argument.type/support :argument.type/undercut})
(s/def :argument/version number?)
(s/def :argument/author ::author)
(s/def :argument/conclusion (s/or :statement ::statement
                                  :argument ::argument))
(s/def :argument/premises (s/coll-of ::statement))
(s/def :argument/discussions (s/coll-of ::discussion))
(s/def ::argument
  (s/keys :req [:argument/author :argument/premises :argument/conclusion
                :argument/type :argument/version]
          :opt [:argument/discussions]))