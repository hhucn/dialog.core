(ns dialog.cli.texts
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
    :argument.type/attack "%s\n\t\"%s\" is not a good idea, because: \"%s\"."
    :argument.type/undercut "%s\n\t\"%s\" does not make sense in the context of \"%s\".\n\tBecause: \"%s\""
    "%s\n\t\"%s\", because: \"%s\"."))

(s/fdef argument-with-author
        :args (s/cat :argument/type :argument/type)
        :ret string?)

(defn statement-with-author
  "Format a statement. Example:
  \"Kangaroo
  We should get a dog\""
  []
  "%s\n\t%s")

(s/fdef statement-with-author
        :ret string?)

(defn reactions
  "Get texts for possible reactions."
  [reaction]
  (get
    {:reaction/undermine "Attack the premise(s) (undermine)"
     :reaction/rebut "Attack the conclusion (rebut)"
     :reaction/undercut "Attack the relation (undercut)"
     :reaction/support "Support the argument's premises with own premises (use argument's premises as conclusion to add own premises)"
     :reaction/defend "Defend my own point of view (add own premises to conclusion)"}
    reaction))

(s/fdef reactions
        :args (s/cat :reaction keyword?)
        :ret string?)

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

(defn- format-undercut
  "Undercuts need to prepare the contents of the attacked argument. Therefore,
  it must be treated differently than a \"regular\" argument."
  [avatar-nickname format-string {:keys [argument/conclusion] :as argument}]
  (let [foreign-conclusion (get-in conclusion [:argument/conclusion :statement/content])
        foreign-premises (concat-premises (:argument/premises conclusion))
        authors-premises (concat-premises (:argument/premises argument))]
    (format format-string
            avatar-nickname
            foreign-conclusion
            foreign-premises
            authors-premises)))

(s/fdef format-undercut
        :args (s/cat :nickname string? :format-string string? :argument ::models/argument)
        :ret string?)

(defn format-argument
  "Prepare String which can be presented to the user based on the provided
  argument."
  [{:argument/keys [author premises conclusion] :as argument}]
  (let [avatar-nickname (avatar-with-nickname (:author/nickname author))
        prepared-premises (concat-premises premises)
        argument-type (:argument/type argument)
        format-string (argument-with-author argument-type)]
    (if (= :argument.type/undercut argument-type)
      (format-undercut avatar-nickname format-string argument)
      (format format-string
              avatar-nickname
              (:statement/content conclusion)
              prepared-premises))))

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

(defn list-options
  "Creates a list of interactive options. Optionally adds an extra option, which
  asks for user input."
  ([options]
   (list-options options false))
  ([options add-new?]
   (let [options' (if add-new? (concat options ["🤟 Add my own statement"]) options)]
     (string/join "\n" (map-indexed
                         (fn [idx content] (format "[%s] %s" idx content))
                         options')))))

(s/fdef list-options
        :args (s/cat :options (s/coll-of string?)
                     :add-new? (s/? boolean?))
        :ret string?)