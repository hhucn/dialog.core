(ns dialog.engine.texts
  "Defines strings, which can be used for text-representation."
  (:require [clojure.string :as string]
            [clojure.spec.alpha :as s]
            [dialog.discussion.models :as models]))

(defn argument-with-author
  "Format an argument. Example:
  \"Kangaroo
  We should get a dog, because: dogs can act as watchdogs.\""
  [argument-type]
  (case argument-type
    :argument.type/attack "%s\n\t%s is not a good idea, because: %s."
    :argument.type/undercut "TODO Undercuts are not yet implemented"
    "%s\n\t%s, because: %s."))

(defn statement-with-author
  "Format a statement. Example:
  \"Kangaroo
  We should get a dog\""
  []
  "%s\n\t%s")

(defn reactions
  "Get texts for possible reactions."
  [reaction]
  (get
    {:reaction/undermine "Attack the premise(s) (undermine)"
     :reaction/rebut "Attack the conclusion (rebut)"
     :reaction/undercut "Attack the relation (undercut)"
     :reaction/support "Support the conclusion with own premises (use argument's premises as conclusion and add own premises)"
     :reaction/defend "Defend my own point of view (add own premises to conclusion)"}
    reaction))

(defn concat-premises
  "Takes a collection of premises and concatenates them."
  [premises]
  (string/join " and " (map :statement/content premises)))

(s/fdef concat-premises
        :args (s/cat :premises (s/coll-of ::models/statement))
        :ret string?)

(defn avatar-with-nickname
  "Pick a random avatar based on the user's nickname. Returns a string, where
  the avatar is prepended to the nickname."
  [nickname]
  (let [avatars ["👩‍🦰" "👨‍🦰" "🎃" "👩‍🦳" "🧔" "🧑" "👨‍🌾" "👩‍🌾" "👨🏿‍🎤" "👩‍🏫" "👨‍💻" "👩‍🎤"
                 "👸" "👾" "🙄" "😬" "🤢" "😈" "👻" "🤓" "🤪"]
        picked (nth avatars (mod (hash nickname) (count avatars)))]
    (format "%s %s" picked nickname)))

(s/fdef avatar-with-nickname
        :args (s/cat :nickname string?)
        :ret string?)

(defn format-argument
  "Prepare String which can be presented to the user based on the provided
  argument."
  [{:argument/keys [author premises conclusion] :as argument}]
  (let [avatar-nickname (avatar-with-nickname (:author/nickname author))
        prepared-premises (concat-premises premises)]
    (format (argument-with-author (:argument/type argument))
            avatar-nickname
            (:statement/content conclusion)
            prepared-premises)))

(s/fdef format-argument
        :args (s/cat :argument ::models/argument)
        :ret string?)

(defn format-statement
  "Prepares a statement string, for text representation."
  [{:statement/keys [content author]}]
  (let [avatar-nickname (avatar-with-nickname (:author/nickname author))]
    (format (statement-with-author)
            avatar-nickname
            content)))

(s/fdef format-statement
        :args (s/cat :statement (s/keys :req [:statement/author :statement/content]))
        :ret string?)

(defn format-premises
  "Prepares and concatenates premises for text representation."
  [premises]
  (let [authors (distinct (map #(get-in % [:statement/author :author/nickname]) premises))
        avatar-nickname (avatar-with-nickname (string/join ", " authors))]
    (format (statement-with-author)
            avatar-nickname
            (concat-premises premises))))

(s/fdef format-premises
        :args (s/cat :premises (s/coll-of ::models/statement))
        :ret string?)