(ns dialog.engine.core
  (:require [dialog.discussion.database :as database]
            [dialog.discussion.models :as models]
            [clojure.spec.alpha :as s]))

;; Das was der user angezeigt bekommt
(defmulti ^:private step
          "Execute one step in the discussion. Represents the states / nodes,
          which require user interaction."
          (fn [step _] step))

(defmethod step :discussion/start
  ;; Show all starting arguments of a discussion.
  [_step args]
  (let [arguments (distinct (database/starting-arguments-by-discussion (:discussion/id args)))
        conclusions (distinct (map :argument/conclusion arguments))]
    [[:starting-conclusions/select (merge args {:present/conclusions conclusions})]
     [:starting-argument/new (dissoc args :present/conclusions)]]))

(defmethod step :react-or-select
  ;; The user can either select another premise for the current conclusion to discuss
  ;; or react with their own premise to the current conclusion.
  ;;TODO finish the arguments for each step
  [_step args]
  [[:premises/select :todo]
   [:support/new :todo]
   [:rebut/new :todo]
   ;; if from starting, then nono undercut
   [:undercut/new :todo]])

(defmethod step :react-or-select-starting
  ;; The user can either select another premise for the current conclusion to discuss
  ;; or react with their own premise to the current conclusion.
  ;;TODO finish the arguments for each step
  [_step args]
  [[:premises/select :todo]
   [:support/new :todo]
   [:rebut/new :todo]])

(defmethod step :reactions/present
  ;; The system chose an argument to confront the user. The user can now
  ;; react to that argument.
  [_step args]
  [[:reaction/support args]
   [:reaction/defend args]
   [:reaction/undermine args]
   [:reaction/undercut args]
   [:reaction/rebut args]])

(defmethod step :reasons/present
  ;; A user chose a reaction. The user can now give a reason for their choice
  ;; or select a pre-defined one.
  [_step args]
  [[:reason/select args]
   [:reason/new (dissoc args :present/reasons)]])

(defmethod step :supports/present
  ;; A user chose a reaction. The user can now give a reason for their choice
  ;; or select a pre-defined one.
  [_step args]
  [[:support/select args]
   [:support/new (dissoc args :present/supports)]])

(defmethod step :undermines/present
  ;; User wants to undermine an argument. Present existing ones and add option
  ;; to create new attack.
  [_step args]
  [[:undermine/select args]
   [:undermine/new (dissoc args :present/undermines)]])

(defmethod step :rebuts/present
  [_step args]
  [[:rebut/select args]
   [:rebut/new (dissoc args :present/rebuts)]])

(defmethod step :undercuts/present
  [_step args]
  [[:undercut/select args]
   [:undercut/new (dissoc args :present/undercuts)]])

(defmethod step :defends/present
  [_step args]
  [[:defend/select args]
   [:defend/new (dissoc args :present/defends)]])

(s/fdef step
        :args (s/cat :step keyword?
                     :args map?)
        :ret (s/coll-of (s/tuple keyword? map?)))


;; -----------------------------------------------------------------------------

(defn- find-attacking-argument
  "Choose an attacker of `argument`."
  [argument]
  (let [attacking-arguments (database/get-attackers-for-argument (:db/id argument))]
    (when-not (empty? attacking-arguments)
      (rand-nth attacking-arguments))))

(s/fdef find-attacking-argument
        :args (s/cat :argument ::models/argument)
        :ret ::models/argument)

(defn find-defending-arguments
  "Choose a subset of arguments that defend the users original standpoint."
  [argument]
  (database/support-for-argument (:db/id argument)))

(s/fdef find-defending-arguments
        :args (s/cat :argument ::models/argument)
        :ret (s/coll-of ::models/argument))


;; -----------------------------------------------------------------------------

;; react: Transitions the state based on what the user chose
(defmulti ^:private react
          "Transitions from one state to the other. The payload for the
          transitions is defined in the multimethods. Always provides the next
          state / node when going through the discussion-loop."
          (fn [current-step _args] current-step))

(s/fdef react
        :args (s/cat :current-step keyword? :args map?)
        :ret (s/tuple keyword? map?))


;; -----------------------------------------------------------------------------
;; Starting Arguments

(defmethod react :starting-conclusions/select
  ;; User has seen all starting arguments and now selected one. Present appropriate
  ;; reactions the user can take for that.
  ;TODO argumente richtig setzen
  [_step args]
  [:react-or-select-starting (dissoc args :present/conclusions)])

(defmethod react :starting-argument/new
  ;; User adds own starting argument. This is stored to the database and the
  ;; discussion flow is reset.
  [_step {:keys [discussion/id user/nickname new/starting-argument-conclusion new/starting-argument-premises]
          :as args}]
  (database/add-new-starting-argument! id nickname starting-argument-conclusion starting-argument-premises)
  [:discussion/start (dissoc args
                             :new/starting-argument-conclusion
                             :new/starting-argument-premises)])


;; -----------------------------------------------------------------------------
;; Selections
(defmethod react :premises/select
  ;; User has seen all starting arguments and now selected one. Present appropriate
  ;; reactions the user can take for that.
  ;TODO argumente richtig setzen
  [_step args]
  [:react-or-select (dissoc args :present/conclusions)])

;; -----------------------------------------------------------------------------
;; New Premises

(defmethod react :support/new
  ;; The user has chosen to support the shown conclusion with their own premise.
  ;; TODO argumente richtig setzen
  [_step args]
  [:react-or-select :todo])

(defmethod react :rebut/new
  ;; The user has chosen to attack the shown conclusion with their own premise.
  ;; TODO argumente richtig setzen
  [_step args]
  [:react-or-select :todo])

(defmethod react :undercut/new
  ;; The user has chosen to attack the relation between the shown conclusion and its predecessor
  ;; with their own premise.
  ;; TODO argumente richtig setzen
  [_step args]
  [:react-or-select :todo])


;; -----------------------------------------------------------------------------
;; Comfort Functions

(defn continue-discussion
  "Takes a last step (according to users choice) and calls the appropriate react
  function. Its result is called with step again."
  [current-step args]
  (let [[next-step new-args] (react current-step args)]
    (step next-step new-args)))

(s/fdef continue-discussion
        :args (s/cat :current-step keyword? :args map?)
        :ret vector?)

(defn start-discussion
  "Start with all starting arguments from a discussion."
  [args]
  (step :discussion/start args))

(s/fdef start-discussion
        :args (s/cat :args (s/keys :req [:discussion/id :user/nickname])))


;; -----------------------------------------------------------------------------

(comment
  (start-discussion {:user/nickname "Christian"
                     :discussion/id 17592186045477})

  (continue-discussion :starting-argument/select
                       {:user/nickname "Christian", :discussion/id 17592186045477
                        :new/starting-argument-conclusion "foo" :new/starting-argument-premises ["bar"]})

  (react :starting-argument/select
         {:user/nickname "Christian", :discussion/id 17592186045477})

  :end)