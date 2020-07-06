(ns dialog.engine.core
  (:require [dialog.discussion.database :as database]))

;; -----------------------------------------------------------------------------

;; Das was der user angezeigt bekommt
(defmulti ^:private step
          "Execute one step in the discussion. Represents the states / nodes,
          which require user interaction."
          (fn [step _] step))

(defmethod step :discussion/title
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
  ;; A user chose a reaction. The user can now give a reason for their choice.
  [_step args]
  [[:reason/select args]
   [:reason/new (dissoc args :present/reasons)]])


;; -----------------------------------------------------------------------------

;; TODO
(defn find-attacking-argument
  "Choose an attacker of `argument` based on user's `attitude`."
  [argument]
  ;; Query the db and choose
  argument)

;; TODO
(defn find-argument-for-opinion
  "Choose an argument that defends the users opinion based on their `attitude`
  towards the current `argument` they are looking at."
  [_attitude argument]
  argument)

;; TODO
(defn find-defending-arguments
  "Choose a subset of arguments that defend the users original standpoint."
  [argument]
  [argument])

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
  [_step {:keys [discussion/title discussion/is-start? user/attitude argument/chosen]
          :as args}]
  (let [arguments (if is-start?
                    (database/starting-arguments-by-title title)
                    (find-argument-for-opinion attitude chosen))]
    [:arguments/present (merge (dissoc args :discussion/is-start?)
                               {:present/arguments arguments})]))

(defmethod react :argument/chosen
  ;; User has chosen an argument and the system is now attacking it. This step
  ;; chooses the attacking argument.
  [_step args]
  (let [attacking-argument (find-attacking-argument (:argument/chosen args))]
    [:reactions/present (merge (dissoc args :present/arguments)
                               {:argument/attacking attacking-argument})]))

(defmethod react :reaction/support
  ;; User has chosen that they support the presented argument. Now, the system
  ;; looks for a new attacking argument.
  [_step args]
  (let [attacking-argument (find-attacking-argument (:argument/chosen args))]
    [:reactions/present (merge args {:argument/attacking attacking-argument
                                     :user/attitude :attitude/pro})]))

(defmethod react :reaction/defend
  ;; User accepts the presented argument, BUT still wants to defend their own
  ;; opinion. Choose all arguments from the step before that represent the users
  ;; opinion.
  [_step {:keys [argument/chosen] :as args}]
  (let [arguments-supporting-user (find-defending-arguments chosen)]
    [:arguments/present (merge args {:present/arguments arguments-supporting-user
                                     :user/attitude :attitude/pro})]))

(defn- generic-attack-reaction
  "Query statements using `f` and return attack relation result needed by
   `next`."
  [f chosen args]
  (let [statements (f chosen)]
    [:reasons/present (merge args {:present/reasons statements
                                   :user/attitude :attitude/pro})]))

(defn statements-attacking-a-premise
  "Returns all statements that attack the premise of `argument`."
  [argument]
  (database/statements-attacking-premise (:db/id argument)))

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

(defmethod react :reason/new
  ;; User can provide a new reason for their attack on the chosen argument.
  [_step {:keys [argument/new] :as args}]
  (let [arguments (find-attacking-argument new)]
    [:arguments/present (merge args {:present/arguments arguments
                                     :user/attitude :attitude/pro})]))

(defmethod react :reason/select
  ;; User finds a suitable reason and selects it.
  [_step args]
  (react :reasons/new args))


(defn continue-discussion
  "Takes a last step (according to users choice) and calls the appropriate react
  function. Its result is called with step again."
  [current-step args]
  (let [[next-step new-args] (react current-step args)]
    (step next-step new-args)))

(defn start-discussion
  [discussion-title]
  (let [[new-step new-args]
        (first (step :discussion/title {:discussion/title discussion-title}))]
    (continue-discussion new-step new-args)))

(comment
  (start-discussion "Cat or Dog?")
  (continue-discussion :argument/chosen
                       (merge {:discussion/title "Cat or Dog?",
                               :discussion/is-start? false,
                               :present/arguments []}
                              {:chosen/argument {:argument/version "hullo"
                                                 :ganz-toll :bar}}))
  (continue-discussion :reaction/undermine
                       {:discussion/title "Cat or Dog?",
                        :chosen/argument {:argument/version "hullo", :ganz-toll :bar},
                        :argument/attacking {:tolles :_argument}})
  (continue-discussion :reason/select
                       {:discussion/title "Cat or Dog?",
                        :chosen/argument {:noch-toller :wonderbar},
                        :argument/attacking {:tolles :_argument},
                        :present/reasons [],
                        :user/attitude :attitude/pro}))
;; 1. Startfunktion
;; 2. Step
;; 3. Zeige user Step möglichkeiten [a, b, c]
;; 4. User wählt aus: a
;; 5. Führe reaction mit a aus.
;; 6. Zeige resultierenden step an.
;; 7. Repeat ab 3
;; TODO * Fehlend: Ein Finish state, oder geschicktes weitermachen
;; TODO * Helferfunktionen schreiben bzw. ausrüsten wie es weiter geht, wenn diese keine
;; TODO funktion zurück liefern