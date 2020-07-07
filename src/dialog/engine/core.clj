(ns dialog.engine.core
  (:require [dialog.discussion.database :as database]
            [clojure.spec.alpha :as s]))

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

(defn find-attacking-argument
  "Choose an attacker of `argument`."
  [argument]
  (let [attacking-arguments (database/get-attackers-for-argument (:db/id argument))]
    (rand-nth attacking-arguments)))

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

(defn statements-attacking-a-conclusion
  "Returns all statements that attack the conclusion of `argument`."
  [argument]
  (database/statements-attacking-conclusion (:db/id argument)))

(defn statements-undercutting-argument
  "Returns all statements that are used to undercut `argument`."
  [argument]
  (database/statements-undercutting-argument (:db/id argument)))

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
  [discussion-title]
  (let [[new-step new-args]
        (first (step :discussion/title {:discussion/title discussion-title}))]
    (continue-discussion new-step new-args)))
(s/fdef start-discussion
        :args (s/cat :discussion-title string?))

(defn choose-argument [argument args]
  (continue-discussion
    :argument/chosen
    (merge args
           {:argument/chosen argument
            :discussion/is-start? false
            :present/arguments []})))

(defn reaction-support [argument args]
  (continue-discussion
    :reaction/support
    (merge args
           {:argument/chosen argument})))


;; -----------------------------------------------------------------------------

(comment
  (declare argument-attacking-for-support argument)

  (start-discussion "Cat or Dog?")
  (choose-argument argument {:discussion/title "Cat or Dog?"})
  (reaction-support argument-attacking-for-support {:discussion/title "Cat or Dog?"})

  (continue-discussion :reaction/undermine
                       {:discussion/title "Cat or Dog?",
                        :chosen/argument {:argument/version "hullo", :ganz-toll :bar},
                        :argument/attacking {:tolles :_argument}})

  (continue-discussion :reason/select
                       {:discussion/title "Cat or Dog?",
                        :chosen/argument {:noch-toller :wonderbar},
                        :argument/attacking {:tolles :_argument},
                        :present/reasons [],
                        :user/attitude :attitude/pro})

  (def nach-start-discussion
    [[:argument/chosen
      {:discussion/title "Cat or Dog?",
       :present/arguments [{:db/id 17592186045432,
                            :argument/version 1,
                            :argument/author #:author{:nickname "Wegi"},
                            :argument/type :argument.type/support,
                            :argument/premises [{:db/id 17592186045433,
                                                 :statement/content "dogs can act as watchdogs",
                                                 :statement/version 1,
                                                 :statement/author #:author{:nickname "Wegi"}}],
                            :argument/conclusion {:db/id 17592186045429,
                                                  :statement/content "we should get a dog",
                                                  :statement/version 1,
                                                  :statement/author #:author{:nickname "Wegi"}}}
                           {:db/id 17592186045434,
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
                                                  :statement/author #:author{:nickname "Wegi"}}}
                           {:db/id 17592186045440,
                            :argument/version 1,
                            :argument/author #:author{:nickname "Christian"},
                            :argument/type :argument.type/support,
                            :argument/premises [{:db/id 17592186045441,
                                                 :statement/content "it would be no problem",
                                                 :statement/version 1,
                                                 :statement/author #:author{:nickname "Christian"}}],
                            :argument/conclusion {:db/id 17592186045431,
                                                  :statement/content "we could get both, a dog and a cat",
                                                  :statement/version 1,
                                                  :statement/author #:author{:nickname "Christian"}}}
                           {:db/id 17592186045444,
                            :argument/version 1,
                            :argument/author #:author{:nickname "Der miese Peter"},
                            :argument/type :argument.type/undercut,
                            :argument/premises [{:db/id 17592186045445,
                                                 :statement/content "won't be best friends",
                                                 :statement/version 1,
                                                 :statement/author #:author{:nickname "Der miese Peter"}}
                                                {:db/id 17592186045446,
                                                 :statement/content "a cat and a dog will generally not get along well",
                                                 :statement/version 1,
                                                 :statement/author #:author{:nickname "Der miese Peter"}}],
                            :argument/conclusion #:db{:id 17592186045440}}
                           {:db/id 17592186045447,
                            :argument/version 1,
                            :argument/author #:author{:nickname "Der Schredder"},
                            :argument/type :argument.type/support,
                            :argument/premises [{:db/id 17592186045448,
                                                 :statement/content "cats are very independent",
                                                 :statement/version 1,
                                                 :statement/author #:author{:nickname "Der Schredder"}}],
                            :argument/conclusion {:db/id 17592186045430,
                                                  :statement/content "we should get a cat",
                                                  :statement/version 1,
                                                  :statement/author #:author{:nickname "Der Schredder"}}}
                           {:db/id 17592186045459,
                            :argument/version 1,
                            :argument/author #:author{:nickname "Der Schredder"},
                            :argument/type :argument.type/support,
                            :argument/premises [{:db/id 17592186045460,
                                                 :statement/content "a cat does not cost taxes like a dog does",
                                                 :statement/version 1,
                                                 :statement/author #:author{:nickname "Der Schredder"}}],
                            :argument/conclusion {:db/id 17592186045430,
                                                  :statement/content "we should get a cat",
                                                  :statement/version 1,
                                                  :statement/author #:author{:nickname "Der Schredder"}}}
                           {:db/id 17592186045467,
                            :argument/version 1,
                            :argument/author #:author{:nickname "Wegi"},
                            :argument/type :argument.type/attack,
                            :argument/premises [{:db/id 17592186045468,
                                                 :statement/content "cats are capricious",
                                                 :statement/version 1,
                                                 :statement/author #:author{:nickname "Wegi"}}],
                            :argument/conclusion {:db/id 17592186045430,
                                                  :statement/content "we should get a cat",
                                                  :statement/version 1,
                                                  :statement/author #:author{:nickname "Der Schredder"}}}]}]])

  (def argument
    {:db/id 17592186045432,
     :argument/version 1,
     :argument/author #:author{:nickname "Wegi"},
     :argument/type :argument.type/support,
     :argument/premises [{:db/id 17592186045433,
                          :statement/content "dogs can act as watchdogs",
                          :statement/version 1,
                          :statement/author #:author{:nickname "Wegi"}}],
     :argument/conclusion {:db/id 17592186045429,
                           :statement/content "we should get a dog",
                           :statement/version 1,
                           :statement/author #:author{:nickname "Wegi"}}})

  (def argument-attacking-for-support
    {:db/id 17592186045434,
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
                           :statement/author #:author{:nickname "Wegi"}}})

  :end)
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