(ns dialog.engine.core
  (:require [dialog.discussion.database :as database]
            [dialog.discussion.models :as models]
            [clojure.spec.alpha :as s]))

;; Das was der user angezeigt bekommt
(defmulti ^:private step
          "Execute one step in the discussion. Represents the states / nodes,
          which require user interaction."
          (fn [step _] step))

(defmethod step :discussion/id
  ;; Show all starting arguments of a discussion.
  [_step args]
  (let [arguments (distinct (database/starting-arguments-by-discussion (:discussion/id args)))]
    [[:starting-argument/select (merge args {:present/arguments arguments})]
     [:starting-argument/new (dissoc args :present/arguments)]]))

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

(defmethod react :starting-argument/select
  ;; User has seen all starting arguments and now selected one. Present appropriate
  ;; reactions the user can take for that.
  [_step args]
  [:reactions/present (dissoc args :present/arguments)])

(defmethod react :starting-argument/new
  ;; User adds own starting argument. This is stored to the database and the
  ;; discussion flow is reset.
  [_step {:keys [discussion/id user/nickname new/starting-argument-conclusion new/starting-argument-premises]
          :as args}]
  (database/add-new-starting-argument! id nickname starting-argument-conclusion starting-argument-premises)
  [:discussion/id (dissoc args
                          :new/starting-argument-conclusion
                          :new/starting-argument-premises)])


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
  [_step {:keys [discussion/id user/nickname new/support argument/chosen]
          :as args}]
  (database/support-argument! id nickname chosen support)
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
  [_step {:keys [argument/chosen discussion/id user/nickname new/undermine]
          :as args}]
  (database/undermine-argument! id nickname chosen undermine)
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
  [_step {:keys [argument/chosen discussion/id user/nickname new/rebut starting-argument?]
          :as args}]
  (let [attacking-argument (find-attacking-argument chosen)
        new-argument-id (database/rebut-argument! id nickname chosen rebut)]
    (when starting-argument?
      (database/set-argument-as-starting! id new-argument-id))
    [:reactions/present (merge (dissoc args :new/rebut :present/rebuts :starting-argument?)
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
  ;; is chosen for the user. `new/undercut` is a collection of strings.
  [_step {:keys [argument/chosen new/undercut discussion/id user/nickname]
          :as args}]
  (database/undercut-argument! id nickname chosen undercut)
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
    [:defends/present (merge args {:present/defends arguments-supporting-user})]))

(defmethod react :defend/select
  ;; User selected an existing defend from a user. This could be
  ;; stored or noticed somewhere. Next, the system searches an attacking
  ;; argument.
  [_step args]
  (let [attacking-argument (find-attacking-argument (:argument/chosen args))
        _selected-defend (:defend/selected args)]

    [:defends/present (merge (dissoc args :new/defend :present/defends)
                             {:argument/chosen attacking-argument})]))

(defmethod react :defend/new
  ;; User provided a new rebut. This needs to be stored and a new argument
  ;; is chosen for the user.
  [_step {:keys [argument/chosen new/defend discussion/id user/nickname starting-argument?]
          :as args}]
  (let [attacking-argument (find-attacking-argument chosen)
        new-argument-id (database/defend-argument! id nickname chosen defend)]
    (when starting-argument?
      (database/set-argument-as-starting! id new-argument-id))
    [:defends/present (merge (dissoc args :new/defend :present/defends :starting-argument?)
                             {:argument/chosen attacking-argument})]))


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
  (step :discussion/id args))

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