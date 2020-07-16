(ns dialog.engine.core
  (:require [dialog.discussion.database :as database]
            [clojure.spec.alpha :as s]))

;; Das was der user angezeigt bekommt
(defmulti ^:private step
          "Execute one step in the discussion. Represents the states / nodes,
          which require user interaction."
          (fn [step _] step))

(defmethod step :discussion/id
  ;; Show all starting arguments of a discussion.
  [_step args]
  [[:arguments/subset (merge args {:discussion/is-start? true})]])

(defmethod step :arguments/present
  ;; A list of arguments is presented to the user. The user can as a next step
  ;; choose an argument from that list.
  [_step args]
  [[:argument/chosen args]])

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

;; -----------------------------------------------------------------------------

(defn find-attacking-argument
  "Choose an attacker of `argument`."
  [argument]
  (let [attacking-arguments (database/get-attackers-for-argument (:db/id argument))]
    (when-not (empty? attacking-arguments)
      (rand-nth attacking-arguments))))

;; TODO
(defn find-argument-for-opinion
  "Choose an argument that defends the users opinion based on their `attitude`
  towards the current `argument` they are looking at."
  [_attitude argument]
  argument)

(defn find-defending-arguments
  "Choose a subset of arguments that defend the users original standpoint."
  [argument]
  (database/support-for-argument (:db/id argument)))

;; react: Transitions the state based on what the user chose
(defmulti ^:private react
          "Transitions from one state to the other. The payload for the
          transitions is defined in the multimethods. Always provides the next
          state / node when going through the discussion-loop."
          (fn [current-step _args] current-step))

(defmethod react :arguments/subset
  ;; Chooses the arguments presented to the user. `discussion/is-start?`
  ;; marks whether those are starting arguments. If not, the arguments
  ;; presented attack `arguments/chosen` based on `user/attitude`.
  [_step {:keys [discussion/id discussion/is-start? user/attitude argument/chosen]
          :as args}]
  (let [arguments (if is-start?
                    (database/starting-arguments-by-discussion id)
                    (find-argument-for-opinion attitude chosen))]
    [:arguments/present (merge (dissoc args :discussion/is-start?)
                               {:present/arguments arguments})]))

(defmethod react :argument/chosen
  [_step args]
  ;; User has chosen an argument and the system is now asking for reactions.
  [:reactions/present (dissoc args :present/arguments)])


;; -----------------------------------------------------------------------------
;; Supports

(defmethod react :reaction/support
  ;; User has chosen that they support the presented argument. Now, the user can
  ;; choose of existing premises or bring own ones.
  [_step args]
  (let [supporting-arguments (database/arguments-supporting-premises
                               (get-in args [:argument/chosen :db/id]))]
    [:supports/present (merge args {:present/supports supporting-arguments})]))

(defmethod react :support/new
  ;; User provided a new support. This needs to be stored and presented a new
  ;; argument to the user.
  [_step {:keys [discussion/id user/nickname new/support argument/chosen] :as args}]
  (database/new-premises-for-argument! id nickname chosen support)
  (let [attacking-argument (find-attacking-argument chosen)]
    [:reactions/present (merge (dissoc args :new/support :present/supports)
                               {:argument/chosen attacking-argument})]))

(defmethod react :support/select
  ;; User selected an existing support from a different user. This could be
  ;; stored or noticed somewhere. Next the system searches an attacking
  ;; argument.
  [_step args]
  (let [attacking-argument (find-attacking-argument (:argument/chosen args))
        _selected-support (:support/selected args)]
    [:reactions/present (merge (dissoc args :new/support :present/supports)
                               {:argument/chosen attacking-argument})]))


;; -----------------------------------------------------------------------------
;; Undermines

(defmethod react :reaction/undermine
  ;; User wants to attack the premises of the shown `argument/chosen`.
  [_step {:keys [argument/chosen] :as args}]
  (let [statements (database/statements-attacking-premise (:db/id chosen))]
    [:undermines/present (merge args {:present/undermines statements})]))

(defmethod react :undermine/select
  ;; User selected an existing undermine from a user. This could be
  ;; stored or noticed somewhere. Next the system searches an attacking
  ;; argument.
  ;; Currently, we do nothing with the selected undermine.
  [_step args]
  (let [attacking-argument (find-attacking-argument (:argument/chosen args))
        _selected-undermine (:undermine/selected args)]
    [:reactions/present (merge (dissoc args :new/undermine :present/undermines)
                               {:argument/chosen attacking-argument})]))

(defmethod react :undermine/new
  ;; User provided a new undermine. This needs to be stored and a new argument
  ;; is chosen for the user
  ;; TODO: Store new undermine to database
  [_step {:keys [new/undermine argument/chosen] :as args}]
  (let [attacking-argument (find-attacking-argument chosen)]
    [:reactions/present (merge (dissoc args :new/undermine :present/undermines)
                               {:argument/chosen attacking-argument})]))


;; -----------------------------------------------------------------------------
;; Rebuts

(defmethod react :reaction/rebut
  [_step {:keys [argument/chosen] :as args}]
  (let [rebuts (database/statements-attacking-conclusion (:db/id chosen))]
    [:rebuts/present (merge args {:present/rebuts rebuts})]))

(defmethod react :rebut/select
  ;; User selected an existing rebut from a user. This could be
  ;; stored or noticed somewhere. Next the system searches an attacking
  ;; argument.
  [_step args]
  (let [attacking-argument (find-attacking-argument (:argument/chosen args))
        _selected-rebut (:rebut/selected args)]
    [:reactions/present (merge (dissoc args :new/rebut :present/rebuts)
                               {:argument/chosen attacking-argument})]))

(defmethod react :rebut/new
  ;; User provided a new rebut. This needs to be stored and a new argument
  ;; is chosen for the user.
  ;; TODO: Store new rebut to database
  [_step {:keys [new/rebut argument/chosen] :as args}]
  (let [attacking-argument (find-attacking-argument chosen)]
    [:reactions/present (merge (dissoc args :new/rebut :present/rebuts)
                               {:argument/chosen attacking-argument})]))


;; -----------------------------------------------------------------------------
;; Undercuts

(defmethod react :reaction/undercut
  [_step {:keys [argument/chosen] :as args}]
  (let [undercuts (database/statements-undercutting-argument (:db/id chosen))]
    [:undercuts/present (merge args {:present/undercuts undercuts})]))

(defmethod react :undercut/select
  ;; User selected an existing undercut from a user. This could be
  ;; stored or noticed somewhere. Next the system searches an attacking
  ;; argument.
  [_step args]
  (let [attacking-argument (find-attacking-argument (:argument/chosen args))
        _selected-undercut (:undercut/selected args)]
    [:reactions/present (merge (dissoc args :new/undercut :present/undercuts)
                               {:argument/chosen attacking-argument})]))

(defmethod react :undercut/new
  ;; User provided a new rebut. This needs to be stored and a new argument
  ;; is chosen for the user.
  ;; TODO: Store new undercut to database
  [_step {:keys [new/undercut argument/chosen] :as args}]
  (let [attacking-argument (find-attacking-argument chosen)]
    [:reactions/present (merge (dissoc args :new/undercut :present/undercuts)
                               {:argument/chosen attacking-argument})]))

;; -----------------------------------------------------------------------------
;; Defend own position

(defmethod react :reaction/defend
  ;; User accepts the presented argument, BUT still wants to defend their own
  ;; opinion. Choose all arguments from the step before that represent the users
  ;; opinion.
  [_step {:keys [argument/chosen] :as args}]
  (let [arguments-supporting-user (find-defending-arguments chosen)]
    [:arguments/present (merge args {:present/arguments arguments-supporting-user
                                     :user/attitude :attitude/pro})]))


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
  (let [[new-step new-args]
        (first (step :discussion/id args))]
    (continue-discussion new-step new-args)))

(s/fdef start-discussion
        :args (s/cat :args (s/keys :req [:discussion/id :user/nickname])))


;; -----------------------------------------------------------------------------

(comment
  (start-discussion {:user/nickname "Christian"
                     :discussion/id 17592186045477})
  :end)