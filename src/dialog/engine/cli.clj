(ns dialog.engine.cli
  (:require [dialog.discussion.database :as database]
            [dialog.engine.core :as engine]
            [dialog.discussion.models :as models]
            [dialog.engine.texts :as texts]
            [clojure.string :as string]
            [clojure.spec.alpha :as s]))

(defn- list-options
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
    (println (list-options (map format-statement conclusions)))
    (let [index (Integer/parseInt (read-line))
          conclusion (nth conclusions index)
          arguments (database/all-arguments-for-conclusion (:db/id conclusion))]
      (choose-argument next-step arguments args))))

(defn- ask-for-new-support [argument]
  (println "Please define why you want to support the following statement:\n")
  (println (format-premises (:argument/premises argument)) "\n")
  (println "Now, how do you want to support this statement?")
  (let [new-premise (read-line)]
    (when (confirmed?)
      (println "🎉 You entered" new-premise)
      new-premise)))
(s/fdef ask-for-new-support
        :args (s/cat :argument ::models/argument))


(defmulti convert-options (fn [reaction _args] reaction))

(defmethod convert-options :support/select
  [step {:keys [present/supports] :as args}]
  (if (empty? supports)
    (convert-options :support/new args)
    (let [formatted-premises (map format-premises (map :argument/premises supports))]
      (println "There are already some supports. Choose an existing one or provide a new support:")
      (println (list-options formatted-premises true))
      (let [option (Integer/parseInt (read-line))]
        (if (= (count supports) option)
          (convert-options :support/new args)               ;; last option selected, which is "add a new support"
          (engine/continue-discussion step (merge args {:support/selected (nth supports option)})))))))

(defmethod convert-options :support/new
  ;; Provide own premise for selected argument.
  [step {:keys [argument/chosen] :as args}]
  (let [new-premise (ask-for-new-support chosen)]
    (if (empty? new-premise)
      (convert-options :support/select args)
      (engine/continue-discussion step (merge args {:new/support new-premise})))))

(defn react-to-argument
  "Extract the information from the engine to formulate argument reaction
  options."
  [args]
  (let [attacking-argument (format-argument (-> args first second :argument/chosen))
        reaction-options (map first args)
        reaction-texts (list-options (map texts/reactions reaction-options))]
    (println "So, you want to talk about this argument:\n")
    (println attacking-argument "\n")
    (println "What do you want to do?")
    (println reaction-texts)
    (let [option (Integer/parseInt (read-line))
          [next-step new-args] (nth args option)]
      (engine/continue-discussion next-step new-args))))

(s/fdef react-to-argument
        :args (s/cat :args ::args))

(defn dispatch-next-step
  "Takes a response from the dialog-engine and selects the most preferable
  option if there are multiple choices."
  [response]
  (let [possible-steps (set (map first response))
        response-lookupable (into {} response)]
    (cond
      (contains? possible-steps :support/select) (convert-options :support/select (:support/select response-lookupable))
      (contains? possible-steps :reaction/support) (react-to-argument response)
      :else (println response))))

(s/def ::args (s/coll-of (s/tuple keyword? map?)))

(defn run
  "Endless loop running the CLI."
  []
  (loop [args (choose-starting-point (start))]
    (recur (dispatch-next-step args))))

(comment
  (require '[clojure.spec.test.alpha :as stest])
  (stest/instrument)

  (require '[clojure.tools.trace :as trace])
  (trace/trace-ns 'dialog.engine.core)

  (-> (start)
      choose-starting-point
      react-to-argument
      dispatch-next-step)

  (declare reaction-args)
  (react-to-argument reaction-args)

  ;; ---------------------------------------------------------------------------

  (def stuff-for-support
    [[:support/select
      {:argument/chosen {:db/id 17592186045434,
                         :argument/version 1,
                         :argument/author #:author{:nickname "Der Schredder"},
                         :argument/type :argument.type/attack,
                         :argument/premises [{:db/id 17592186045435,
                                              :statement/content "you have to take the dog for a walk every day, which is tedious",
                                              :statement/version 1,
                                              :statement/author #:author{:nickname "Der Schredder"}}],
                         :argument/conclusion {:db/id 17592186045429,
                                               :statement/content "we should get a dog",
                                               :statement/version 1,
                                               :statement/author #:author{:nickname "Wegi"}}},
       :discussion/id 17592186045477,
       :user/nickname "Christian",
       :discussion/title "Cat or Dog?",
       :present/supports (),
       :current/reaction :reaction/support}]
     [:support/new
      {:argument/chosen {:db/id 17592186045434,
                         :argument/version 1,
                         :argument/author #:author{:nickname "Der Schredder"},
                         :argument/type :argument.type/attack,
                         :argument/premises [{:db/id 17592186045435,
                                              :statement/content "you have to take the dog for a walk every day, which is tedious",
                                              :statement/version 1,
                                              :statement/author #:author{:nickname "Der Schredder"}}],
                         :argument/conclusion {:db/id 17592186045429,
                                               :statement/content "we should get a dog",
                                               :statement/version 1,
                                               :statement/author #:author{:nickname "Wegi"}}},
       :discussion/id 17592186045477,
       :user/nickname "Christian",
       :discussion/title "Cat or Dog?",
       :present/supports (),
       :current/reaction :reaction/support}]])

  (def reaction-args
    [[:reaction/support
      {:argument/chosen {:db/id 17592186045434,
                         :argument/version 1,
                         :argument/author #:author{:nickname "Der Schredder"},
                         :argument/type :argument.type/attack,
                         :argument/premises [{:db/id 17592186045435,
                                              :statement/content "you have to take the dog for a walk every day, which is tedious",
                                              :statement/version 1,
                                              :statement/author #:author{:nickname "Der Schredder"}}],
                         :argument/conclusion {:db/id 17592186045429,
                                               :statement/content "we should get a dog",
                                               :statement/version 1,
                                               :statement/author #:author{:nickname "Wegi"}}},
       :discussion/id 17592186045477,
       :user/nickname "Christian",
       :discussion/title "Cat or Dog?"}]
     [:reaction/defend
      {:argument/chosen {:db/id 17592186045434,
                         :argument/version 1,
                         :argument/author #:author{:nickname "Der Schredder"},
                         :argument/type :argument.type/attack,
                         :argument/premises [{:db/id 17592186045435,
                                              :statement/content "you have to take the dog for a walk every day, which is tedious",
                                              :statement/version 1,
                                              :statement/author #:author{:nickname "Der Schredder"}}],
                         :argument/conclusion {:db/id 17592186045429,
                                               :statement/content "we should get a dog",
                                               :statement/version 1,
                                               :statement/author #:author{:nickname "Wegi"}}},
       :discussion/id 17592186045477,
       :user/nickname "Christian",
       :discussion/title "Cat or Dog?"}]
     [:reaction/undermine
      {:argument/chosen {:db/id 17592186045434,
                         :argument/version 1,
                         :argument/author #:author{:nickname "Der Schredder"},
                         :argument/type :argument.type/attack,
                         :argument/premises [{:db/id 17592186045435,
                                              :statement/content "you have to take the dog for a walk every day, which is tedious",
                                              :statement/version 1,
                                              :statement/author #:author{:nickname "Der Schredder"}}],
                         :argument/conclusion {:db/id 17592186045429,
                                               :statement/content "we should get a dog",
                                               :statement/version 1,
                                               :statement/author #:author{:nickname "Wegi"}}},
       :discussion/id 17592186045477,
       :user/nickname "Christian",
       :discussion/title "Cat or Dog?"}]
     [:reaction/undercut
      {:argument/chosen {:db/id 17592186045434,
                         :argument/version 1,
                         :argument/author #:author{:nickname "Der Schredder"},
                         :argument/type :argument.type/attack,
                         :argument/premises [{:db/id 17592186045435,
                                              :statement/content "you have to take the dog for a walk every day, which is tedious",
                                              :statement/version 1,
                                              :statement/author #:author{:nickname "Der Schredder"}}],
                         :argument/conclusion {:db/id 17592186045429,
                                               :statement/content "we should get a dog",
                                               :statement/version 1,
                                               :statement/author #:author{:nickname "Wegi"}}},
       :discussion/id 17592186045477,
       :user/nickname "Christian",
       :discussion/title "Cat or Dog?"}]
     [:reaction/rebut
      {:argument/chosen {:db/id 17592186045434,
                         :argument/version 1,
                         :argument/author #:author{:nickname "Der Schredder"},
                         :argument/type :argument.type/attack,
                         :argument/premises [{:db/id 17592186045435,
                                              :statement/content "you have to take the dog for a walk every day, which is tedious",
                                              :statement/version 1,
                                              :statement/author #:author{:nickname "Der Schredder"}}],
                         :argument/conclusion {:db/id 17592186045429,
                                               :statement/content "we should get a dog",
                                               :statement/version 1,
                                               :statement/author #:author{:nickname "Wegi"}}},
       :discussion/id 17592186045477,
       :user/nickname "Christian",
       :discussion/title "Cat or Dog?"}]])


  :end)