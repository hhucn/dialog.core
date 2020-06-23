(ns dialog.engine.core
  (:require [dialog.discussion.database :as database]))

(def ... :todo)

(defmulti next (fn [step-or-reaction args] step-or-reaction))
(defmethod next :step
  [_step _args])

(defmethod next :reaction
  [_reaction _args])


;; -----------------------------------------------------------------------------

;; Das was der user angezeigt bekommt
(defmulti step (fn [step args] step))

(defmethod step :discussion/title
  ;; Show all starting arguments of a discussion.
  [_step args]
  [[:arguments/subset args]])

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
  ;; A user chose a reaction. The user can now give a reason for their choice.
  [_step args]
  [[:reason/select args]
   [:reason/new args]])


;; -----------------------------------------------------------------------------

;; TODO
(defn find-attacking-argument
  "Choose an attacker of `argument` based on user's `attitude`."
  [attitude argument]
  ;; Query the db and choose
  argument)

;; TODO
(defn find-argument-for-opinion
  "Choose an argument that defends the users opinion based on their `attitude`
  towards the current `argument` they are looking at."
  [attitude argument]
  argument)

;; TODO
(defn find-defending-arguments
  "Choose a subset of arguments that defend the users original standpoint."
  [attitude argument]
  [argument])

;; Das was der user auswählt
(defmulti react (fn [current-step reaction] reaction))

(defmethod react :arguments/subset
  ;; Chooses the arguments presented to the user. `discussion/is-start?`
  ;; marks whether those are starting arguments. If not, the arguments
  ;; presented attack `arguments/chosen` based on `user/attitude`.
  [_step {:keys [discussion/title discussion/is-start? user/attitude argument/chosen]
          :as args}]
  (let [arguments (if is-start?
                    (database/starting-arguments-by-title title)
                    (find-argument-for-opinion attitude chosen))]
    [[:arguments/present (merge args {:arguments/present arguments})]]))

(defmethod react :argument/chosen
  ;; User has chosen an argument and the system is now attacking for it. This step
  ;; chooses the attacking argument.
  [_step args]
  (let [attacking-argument (find-attacking-argument (:user/attitude args)
                                                    (:argument/chosen args))]
    [[:reactions/present (merge args {:argument/attacking attacking-argument})]]))

(defmethod react :reaction/support
  ;; User has chosen that they support the presented argument. Now, the system
  ;; looks for a new attacking argument.
  [_step args]
  (let [attacking-argument (find-attacking-argument :attitude/pro
                                                    (:argument/chosen args))]
    [[:reactions/present (merge args {:argument/attacking attacking-argument
                                      :user/attitude :attitude/pro})]]))

(defmethod react :reaction/defend
  ;; User accepts the presented argument, BUT still wants to defend their own
  ;; opinion. Choose all arguments from the step before that represent the users
  ;; opinion.
  [_step {:keys [argument/chosen] :as args}]
  (let [arguments-supporting-user (find-defending-arguments :attitude/pro chosen)]
    [[:arguments/present (merge args {:arguments/present arguments-supporting-user
                                      :user/attitude :attitude/pro})]]))

(defn- generic-attack-reaction
  "Query statements using `f` and return attack relation result needed by
   `next`."
  [f chosen args]
  (let [statements (f chosen)]
    [[:reasons/present (merge args {:reasons/present statements})]]))

;; TODO
(defn statements-attacking-a-premise
  "Returns all statements that attack the premise of `argument`."
  [argument]
  [])

;; TODO
(defn statements-attacking-a-conclusion
  "Returns all statements that attack the conclusion of `argument`."
  [argument]
  [])

;; TODO
(defn statements-undercutting-argument
  "Returns all statements that are used to undercut `argument`."
  [argument]
  [])

(defmethod react :reaction/undermine
  ;; User wants to attack the premises of the shown `argument/chosen`.
  [_step {:keys [argument/chosen] :as args}]
  (generic-attack-reaction statements-attacking-a-premise chosen args))

(defmethod react :reaction/rebut
  ;; User wants to attack the premises of the shown `argument/chosen`.
  [_step {:keys [argument/chosen] :as args}]
  (generic-attack-reaction statements-attacking-a-conclusion chosen args))

(defmethod react :reaction/undercut
  ;; User wants to attack the premises of the shown `argument/chosen`.
  [_step {:keys [argument/chosen] :as args}]
  (generic-attack-reaction statements-undercutting-argument chosen args))

(defmethod react :reasons/new
  ;; User can provide a new reason for their attack on the chosen argument.
  [_step {:keys [argument/new] :as args}]
  (let [arguments (find-attacking-argument :attitude/pro new)]
    [[:arguments/present (merge args {:arguments/present arguments
                                      :user/attitude :attitude/pro})]]))

(defmethod react :reasons/select
  ;; User finds a suitable reason and selects it.
  [_step args]
  (react :reasons/new args))