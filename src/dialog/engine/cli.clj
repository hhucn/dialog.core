(ns dialog.engine.cli
  (:require [dialog.discussion.database :as database]
            [dialog.engine.core :as engine]
            [dialog.discussion.models :as models]
            [dialog.engine.texts :as texts]
            [clojure.string :as string]
            [clojure.spec.alpha :as s]))

(defn- list-options
  "Creates a list of interactive options."
  [options]
  (string/join "\n" (map-indexed
                      (fn [idx content] (format "[%s] %s" idx content))
                      options)))

(s/fdef list-options
        :args (s/cat :options (s/coll-of string?))
        :ret string?)

(defn- confirmed? []
  (println "Are you satisfied with your input? [y/n]")
  (let [input (.toLowerCase (read-line))]
    (= "y" input)))

(s/fdef confirmed? :ret boolean?)

(defn start []
  (let [discussions (database/all-discussion-titles-and-ids)]
    (println
      "Welcome 🥳! Choose a discussion:\n"
      (list-options (map second discussions)))
    (let [index (Integer/parseInt (read-line))
          [id title] (nth discussions index)]
      {:user/nickname "Christian"
       :discussion/id id
       :discussion/title title})))

(defn- prepare-starting-conclusions
  "Do a step in the discussion engine and query the starting conclusions."
  [args]
  (let [[next-step response] (first (engine/start-discussion args))
        conclusions (map :argument/conclusion (:present/arguments response))]
    {:next-step next-step
     :conclusions (filter #(s/valid? ::models/statement %) (set conclusions))}))

(defn- format-argument
  "Prepare String which can be presented to the user based on the provided
  argument."
  [{:argument/keys [author premises conclusion] :as argument}]
  (let [avatar-nickname (texts/avatar-with-nickname (:author/nickname author))
        prepared-premises (texts/concat-premises premises)]
    (format (texts/argument-with-author (:argument/type argument))
            avatar-nickname
            (:statement/content conclusion)
            prepared-premises)))

(s/fdef format-argument
        :args (s/cat :argument ::models/argument)
        :ret string?)

(defn- format-statement
  "Prepares a statement string, for text representation."
  [{:statement/keys [content author]}]
  (let [avatar-nickname (texts/avatar-with-nickname (:author/nickname author))]
    (format (texts/statement-with-author)
            avatar-nickname
            content)))

(s/fdef format-statement
        :args (s/cat :statement (s/keys :req [:statement/author :statement/content]))
        :ret string?)

(defn- format-premises
  "Prepares and concatenates premises for text representation."
  [premises]
  (let [authors (set (map #(get-in % [:statement/author :author/nickname]) premises))
        avatar-nickname (texts/avatar-with-nickname (string/join ", " authors))]
    (format (texts/statement-with-author)
            avatar-nickname
            (texts/concat-premises premises))))

(s/fdef format-premises
        :args (s/cat :premises (s/coll-of ::models/statement))
        :ret string?)

(defn choose-argument [next-step arguments args]
  (let [argument-strings (list-options (map format-argument arguments))]
    (println "Here are some arguments for your position. Select one to react to it:")
    (println argument-strings))
  (let [index (Integer/parseInt (read-line))
        argument (nth arguments index)]
    (engine/continue-discussion next-step (merge {:argument/chosen argument} args))))

(defn choose-starting-point [args]
  (let [{:keys [next-step conclusions]} (prepare-starting-conclusions args)]
    (println "Choose your starting point:")
    (println (list-options (map :statement/content conclusions)))
    (let [index (Integer/parseInt (read-line))
          conclusion (nth conclusions index)
          arguments (database/all-arguments-for-conclusion (:db/id conclusion))]
      (choose-argument next-step arguments args))))

(comment
  (clojure.spec.test.alpha/instrument)
  (def some-args {:discussion/id 17592186045477
                  :discussion/title "Cat or Dog?"})

  (-> (start)
      choose-starting-point)

  :end)