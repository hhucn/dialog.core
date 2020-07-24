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

(s/fdef step
        :args (s/cat :step keyword?
                     :args map?)
        :ret (s/coll-of (s/tuple keyword? map?)))

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