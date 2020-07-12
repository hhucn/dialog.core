(ns dialog.engine.texts
  "Defines strings, which can be used for text-representation."
  (:require [clojure.string :as string]
            [clojure.spec.alpha :as s]
            [dialog.discussion.models :as models]))

(defn argument
  "Format an argument. Example:
  \"Kangaroo
  We should get a dog, because: dogs can act as watchdogs.\""
  []
  "%s\n%s, because: %s.")

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