(ns dialog.cli.core
  "Small CLI to navigate in the discussion and collect user's input. Useful for
  development purposes."
  (:require [clojure.spec.alpha :as s]
            [dialog.cli.interactions :as interactions]
            [dialog.cli.texts :as texts]
            [dialog.discussion.models :as models]
            [dialog.engine.core :as engine]))


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
    (let [formatted-premises (map texts/format-premises statements)]
      (println instruction)
      (println (texts/list-options formatted-premises true))
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
        (merge args {store-new [new-statement]})))))

(defmethod convert-options :starting-argument/select
  ;; Present the conclusions from the starting arguments to enter the
  ;; discussion.
  [step {:keys [present/arguments] :as args}]
  (let [filtered-arguments (distinct (filter #(not= "argument.type/undercut" (:argument/type %)) arguments))
        conclusions (map :argument/conclusion filtered-arguments)
        unique-starting-points (filter #(s/valid? ::models/statement %) conclusions)]
    (println "Choose your starting point:")
    (println (texts/list-options (map texts/format-statement unique-starting-points)))
    (let [index (Integer/parseInt (read-line))
          chosen-argument (nth filtered-arguments index)]
      (engine/continue-discussion step (merge args {:argument/chosen chosen-argument})))))

(defmethod convert-options :argument/chosen
  ;; After the presentation of the positions, the first arguments are presented,
  ;; from which the user can choose from.
  [step {:keys [present/arguments] :as args}]
  (let [argument-strings (texts/list-options (map texts/format-argument arguments))]
    (println "Here are some arguments for your position. Select one to react to it:")
    (println argument-strings)
    (let [index (Integer/parseInt (read-line))
          argument (nth arguments index)]
      (engine/continue-discussion step (merge args {:argument/chosen argument})))))

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
    interactions/ask-for-new-support chosen args))

(defmethod convert-options :defend/select
  [step {:keys [present/defends] :as args}]
  (generic-collect-input
    step :defend/new :defend/selected (:argument/conclusion defends)
    "Here are the supports for the selected conclusion:"
    args))

(defmethod convert-options :defend/new
  ;; Provide own premise for selected argument.
  [step {:keys [argument/chosen] :as args}]
  (generic-create-new-statements
    step :defend/select :new/defend
    interactions/ask-for-new-defend chosen args))

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
      interactions/ask-for-new-attack formatted-premises args)))

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
      interactions/ask-for-new-attack formatted-statement args)))

(defmethod convert-options :undercut/select
  [step {:keys [present/undermines] :as args}]
  (generic-collect-input
    step :undercut/new :undercut/selected undermines
    "There are already some attacks on the relation / coherence of this
    argument. Choose an existing one or provide a new attack:"
    args))

(defmethod convert-options :undercut/new
  [step {:keys [argument/chosen] :as args}]
  (let [formatted-statement (texts/format-statement (:argument/conclusion chosen))]
    (generic-create-new-statements
      step :undercut/select :new/undercut
      interactions/ask-for-new-attack formatted-statement args)))

(defmethod convert-options :reaction/support
  ;; Collects _all_ possible reactions and prints fitting options to it.
  [_step args]
  (let [attacking-argument (texts/format-argument (-> args first second :argument/chosen))
        reaction-options (map first args)
        reaction-texts (texts/list-options (map texts/reactions reaction-options))]
    (println "So, you want to talk about this argument:\n")
    (println attacking-argument "\n")
    (println "What do you want to do?")
    (println reaction-texts)
    (let [option (Integer/parseInt (read-line))
          [next-step new-args] (nth args option)]
      (engine/continue-discussion next-step new-args))))

(defmethod convert-options :default
  [_step args]
  (throw
    (new UnsupportedOperationException
         (format "Unfortunately, you reached this point 😔 Therefore, you
         received a step from the discussion engine, which is currently not
         implemented by this interface or you just have a typo. Here are the
         provided arguments:
         %s" args))))


;; -----------------------------------------------------------------------------

(defn dispatch-next-step
  "Takes a response from the dialog-engine and selects the most preferable
  option if there are multiple choices."
  [response]
  (let [possible-steps (set (map first response))
        response-lookupable (into {} response)]
    (cond
      (contains? possible-steps :defend/select) (convert-options :defend/select (:defend/select response-lookupable))
      (contains? possible-steps :argument/chosen) (convert-options :argument/chosen (:argument/chosen response-lookupable))
      (contains? possible-steps :starting-argument/select) (convert-options :starting-argument/select (:starting-argument/select response-lookupable))
      (contains? possible-steps :support/select) (convert-options :support/select (:support/select response-lookupable))
      (contains? possible-steps :reaction/support) (convert-options :reaction/support response)
      (contains? possible-steps :undermine/select) (convert-options :undermine/select (:undermine/select response-lookupable))
      (contains? possible-steps :rebut/select) (convert-options :rebut/select (:rebut/select response-lookupable))
      (contains? possible-steps :undercut/select) (convert-options :undercut/select (:undercut/select response-lookupable))
      :else (throw (new UnsupportedOperationException
                        (format "Not implemented. Received: \n%s" response))))))

(defn -main
  "Endless loop running the CLI."
  []
  (loop [args (engine/start-discussion (interactions/start))]
    (recur (dispatch-next-step args))))

(s/def ::args (s/coll-of (s/tuple keyword? map?)))


;; -----------------------------------------------------------------------------

(comment
  (-main)
  (require '[clojure.spec.test.alpha :as stest])
  (stest/instrument)
  (stest/unstrument)

  (require '[clojure.tools.trace :as trace])
  (trace/trace-ns 'dialog.engine.core)

  :end)