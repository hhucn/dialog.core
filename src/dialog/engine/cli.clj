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
        :args (s/cat :options (s/coll-of string?)
                     :add-new? (s/? boolean?))
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

(defn- ask-for-new-input
  "Generic function to ask for user input."
  [formatted-statements introduction input-request]
  (println introduction "\n")
  (println formatted-statements "\n")
  (println input-request)
  (let [new-statement (read-line)]
    (when (confirmed?)
      (println "🎉 You entered" new-statement)
      new-statement)))

(s/fdef ask-for-new-input
        :args (s/cat :formatted-statements string? :introduction string?
                     :input-request string?)
        :ret string?)

(defn- ask-for-new-support [argument]
  (ask-for-new-input
    (format-premises (:argument/premises argument))
    "Please define why you want to support the following statement:"
    "Now, how do you want to support this statement?"))

(s/fdef ask-for-new-support
        :args (s/cat :argument (s/keys :req [:argument/premises])))

(defn- ask-for-new-attack [formatted-statement]
  (ask-for-new-input
    formatted-statement
    "Please define why you want to attack the following statement:"
    "Now, how to you want to attack this statement?"))



;; -----------------------------------------------------------------------------
;; Reactions to options from the discussion engine

(defmulti convert-options
          "Accepts the values from engine's react function, prints some text to
          the user and calls the next step in the discussion engine."
          (fn [reaction _args] reaction))

(defn- generic-collect-input
  "Generic function to present existing statements, which can be selected.
  Redirects to the specific create-new-entities functions and calls the
  execution engine in the end."
  [step create-new-step store-selected statements instruction args]
  (if (empty? statements)
    (convert-options create-new-step args)
    (let [formatted-premises (map format-premises statements)]
      (println instruction)
      (println (list-options formatted-premises true))
      (let [option (Integer/parseInt (read-line))]
        (if (= (count statements) option)
          (convert-options create-new-step args)            ;; last option selected, which is "add a new support"
          (engine/continue-discussion
            step
            (merge args {store-selected (nth statements option)})))))))

(defmethod convert-options :support/select
  [step {:keys [present/supports] :as args}]
  (generic-collect-input
    step :support/new :support/selected (map :argument/premises supports)
    "There are already some supports. Choose an existing one or provide a new support:"
    args))

(defmethod convert-options :support/new
  ;; Provide own premise for selected argument.
  [step {:keys [argument/chosen] :as args}]
  (let [new-premise (ask-for-new-support chosen)]
    (if (empty? new-premise)
      (convert-options :support/select args)
      (engine/continue-discussion step (merge args {:new/support new-premise})))))

(defmethod convert-options :undermine/select
  [step {:keys [present/undermines] :as args}]
  (generic-collect-input
    step :undermine/new :undermine/selected undermines
    "There are already some attacks on the premise. Choose an existing one or provide a new attack:"
    args))

(defmethod convert-options :undermine/new
  [step {:keys [argument/chosen] :as args}]
  (let [new-undermine (ask-for-new-attack (format-premises (:argument/premises chosen)))]
    (if (empty? new-undermine)
      (convert-options :undermine/select args)
      (engine/continue-discussion step (merge args {:new/undermine new-undermine})))))

(defmethod convert-options :rebut/select
  [step {:keys [present/undermines] :as args}]
  (generic-collect-input
    step :rebut/new :rebut/selected undermines
    "There are already some attacks on the conclusion. Choose an existing one or provide a new attack:"
    args))

(defmethod convert-options :rebut/new
  [step {:keys [argument/chosen] :as args}]
  (let [new-rebut (ask-for-new-attack (format-statement (:argument/conclusion chosen)))]
    (if (empty? new-rebut)
      (convert-options :rebut/select args)
      (engine/continue-discussion step (merge args {:new/rebut new-rebut})))))

(defmethod convert-options :undercut/select
  [step {:keys [present/undermines] :as args}]
  (generic-collect-input
    step :undercut/new :undercut/selected undermines
    "There are already some attacks on the argument's relation. Choose an
    existing one or provide a new attack:"
    args))

(defmethod convert-options :undercut/new
  [step {:keys [argument/chosen] :as args}]
  (let [new-undercut (ask-for-new-attack (format-statement (:argument/conclusion chosen)))]
    (if (empty? new-undercut)
      (convert-options :undercut/select args)
      (engine/continue-discussion step (merge args {:new/undercut new-undercut})))))

(defmethod convert-options :default
  [_step args]
  (throw
    (new UnsupportedOperationException
         (format "Unfortunately, you reached this point 😔 Therefore, you
         received a step in the discussion engine, which is currently not
         implemented by this interface. Here are the provided arguments:
         %s" args))))


;; -----------------------------------------------------------------------------

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
      (contains? possible-steps :undermine/select) (convert-options :undermine/select (:undermine/select response-lookupable))
      (contains? possible-steps :rebut/select) (convert-options :rebut/select (:rebut/select response-lookupable))
      (contains? possible-steps :undercut/select) (convert-options :undercut/select (:undercut/select response-lookupable))
      :else (throw (new UnsupportedOperationException "Not implemented")))))

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

  :end)