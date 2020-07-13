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