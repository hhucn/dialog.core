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

(defn- start []
  (let [discussions (database/all-discussion-titles-and-ids)]
    (println
      "Welcome 🥳! Choose a discussion:\n"
      (list-options (map second discussions)))
    (let [index (Integer/parseInt (read-line))
          [id title] (nth discussions index)]
      {:user/nickname "Christian"
       :discussion/id id
       :discussion/title title})))

(defn- choose-argument
  "After the presentation of the positions, the first arguments are presented,
  from which the user can choose from."
  [next-step arguments args]
  (let [argument-strings (list-options (map texts/format-argument arguments))]
    (println "Here are some arguments for your position. Select one to react to it:")
    (println argument-strings))
  (let [index (Integer/parseInt (read-line))
        argument (nth arguments index)]
    (engine/continue-discussion next-step (merge args {:argument/chosen argument}))))

(defn- choose-starting-point
  "Present the conclusions from the starting arguments to enter the discussion."
  [next-step args]
  (let [conclusions (map :argument/conclusion (:present/arguments args))
        unique-starting-points (filter #(s/valid? ::models/statement %) (set conclusions))]
    (println "Choose your starting point:")
    (println (list-options (map texts/format-statement unique-starting-points)))
    (let [index (Integer/parseInt (read-line))
          conclusion (nth unique-starting-points index)
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
    (texts/format-premises (:argument/premises argument))
    "Please define why you want to support the following statement:"
    "Now, how do you want to support this statement?"))

(s/fdef ask-for-new-support
        :args (s/cat :argument (s/keys :req [:argument/premises]))
        :ret string?)

(defn- ask-for-new-attack [formatted-statement]
  (ask-for-new-input
    formatted-statement
    "Please define why you want to attack the following statement:"
    "Now, how to you want to attack this statement?"))

(s/fdef ask-for-new-attack
        :args (s/cat :formatted-statement string?)
        :ret string?)


;; -----------------------------------------------------------------------------
;; Reactions to options from the discussion engine

(defmulti convert-options
          "Accepts the values from engine's react function, prints some text to
          the user and calls the next step in the discussion engine."
          (fn [reaction _args] reaction))

(s/fdef convert-options
        :args (s/cat :reaction keyword? :args ::args))

(defn- generic-collect-input
  "Generic function to present existing statements, which can be selected.
  Redirects to the specific create-new-entities functions and calls the
  execution engine in the end."
  [step create-new-step store-selected statements instruction args]
  (if (empty? statements)
    (convert-options create-new-step args)
    (let [formatted-premises (map texts/format-premises statements)]
      (println instruction)
      (println (list-options formatted-premises true))
      (let [option (Integer/parseInt (read-line))]
        (if (= (count statements) option)
          (convert-options create-new-step args)            ;; last option selected, which is "add a new support"
          (engine/continue-discussion
            step
            (merge args {store-selected (nth statements option)})))))))

(defn- generic-create-new-statements
  "Ask for new input from the user and calls the next step in the discussion."
  [step select-key store-new ask-fn formatted-statement args]
  (let [new-statement (ask-fn formatted-statement)]
    (if (empty? new-statement)
      (convert-options select-key args)
      (engine/continue-discussion
        step
        (merge args {store-new new-statement})))))

(defmethod convert-options :support/select
  [step {:keys [present/supports] :as args}]
  (generic-collect-input
    step :support/new :support/selected (map :argument/premises supports)
    "There are already some supports. Choose an existing one or provide a new support:"
    args))

(defmethod convert-options :support/new
  ;; Provide own premise for selected argument.
  [step {:keys [argument/chosen] :as args}]
  (generic-create-new-statements
    step :support/select :new/support
    ask-for-new-support chosen args))

(defmethod convert-options :undermine/select
  [step {:keys [present/undermines] :as args}]
  (generic-collect-input
    step :undermine/new :undermine/selected undermines
    "There are already some attacks on the premise. Choose an existing one or provide a new attack:"
    args))

(defmethod convert-options :undermine/new
  [step {:keys [argument/chosen] :as args}]
  (let [formatted-premises (texts/format-premises (:argument/premises chosen))]
    (generic-create-new-statements
      step :undermine/select :new/undermine
      ask-for-new-attack formatted-premises args)))

(defmethod convert-options :rebut/select
  [step {:keys [present/undermines] :as args}]
  (generic-collect-input
    step :rebut/new :rebut/selected undermines
    "There are already some attacks on the conclusion. Choose an existing one or provide a new attack:"
    args))

(defmethod convert-options :rebut/new
  [step {:keys [argument/chosen] :as args}]
  (let [formatted-statement (texts/format-statement (:argument/conclusion chosen))]
    (generic-create-new-statements
      step :rebut/select :new/rebut
      ask-for-new-attack formatted-statement args)))

(defmethod convert-options :undercut/select
  [step {:keys [present/undermines] :as args}]
  (generic-collect-input
    step :undercut/new :undercut/selected undermines
    "There are already some attacks on the argument's relation. Choose an
    existing one or provide a new attack:"
    args))

(defmethod convert-options :undercut/new
  [step {:keys [argument/chosen] :as args}]
  (let [formatted-statement (texts/format-statement (:argument/conclusion chosen))]
    (generic-create-new-statements
      step :undercut/select :new/undercut
      ask-for-new-attack formatted-statement args)))

(defmethod convert-options :default
  [_step args]
  (throw
    (new UnsupportedOperationException
         (format "Unfortunately, you reached this point 😔 Therefore, you
         received a step in the discussion engine, which is currently not
         implemented by this interface or you just have a typo. Here are the
         provided arguments:
         %s" args))))


;; -----------------------------------------------------------------------------

(defn react-to-argument
  "Extract the information from the engine to formulate argument reaction
  options."
  [args]
  (let [attacking-argument (texts/format-argument (-> args first second :argument/chosen))
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
      (contains? possible-steps :argument/chosen) (choose-starting-point :argument/chosen (:argument/chosen response-lookupable))
      (contains? possible-steps :support/select) (convert-options :support/select (:support/select response-lookupable))
      (contains? possible-steps :reaction/support) (react-to-argument response)
      (contains? possible-steps :undermine/select) (convert-options :undermine/select (:undermine/select response-lookupable))
      (contains? possible-steps :rebut/select) (convert-options :rebut/select (:rebut/select response-lookupable))
      (contains? possible-steps :undercut/select) (convert-options :undercut/select (:undercut/select response-lookupable))
      :else (throw (new UnsupportedOperationException
                        (format "Not implemented. Received: \n%s" response))))))

(s/def ::args (s/coll-of (s/tuple keyword? map?)))

(defn run
  "Endless loop running the CLI."
  []
  (loop [args (engine/start-discussion (start))]
    (recur (dispatch-next-step args))))


;; -----------------------------------------------------------------------------

(comment
  (require '[clojure.spec.test.alpha :as stest])
  (stest/instrument)

  (require '[clojure.tools.trace :as trace])
  (trace/trace-ns 'dialog.engine.core)

  (-> (start)
      choose-starting-point
      react-to-argument
      dispatch-next-step)
  :end)