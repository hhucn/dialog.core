(ns dialog.engine.core
  (:require [dialog.discussion.database :as database]
            [clojure.spec.alpha :as s]
            [ghostwheel.core :refer [>defn-]]
            [dialog.discussion.models :as models]))

(>defn- build-meta-premises
  "Builds a meta-premise with additional information for the frontend out of a
  list of arguments."
  [arguments]
  [(s/coll-of ::models/argument)
   :ret (s/coll-of ::models/statement)]
  (flatten
    (map (fn [args]
           (map (fn [premise] (assoc premise :meta/argument.type (:argument/type args)))
                (:argument/premises args)))
         arguments)))

;; -----------------------------------------------------------------------------

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
  [_step args]
  ;; First go one step further, by setting the last premise as the next conclusion
  ;; This way the next chosen premise is still correctly matching it.
  (let [updated-args (-> args
                         (assoc :conclusion/chosen (:premise/chosen args))
                         (dissoc :premise/chosen))
        add-premise-args (dissoc updated-args :present/premises :present/undercuts)
        ;; Get the id of the argument which can be undercut.
        undercut-id (database/argument-id-by-premise-conclusion (:premise/chosen args) (:conclusion/chosen args))]
    [[:premises/select updated-args]
     [:support/new add-premise-args]
     [:rebut/new add-premise-args]
     [:undercut/new (assoc add-premise-args :undercut/argument-id undercut-id)]]))

(defmethod step :react-or-select-after-addition
  ;; The user can either select another premise for the current conclusion to discuss
  ;; or react with their own premise to the current conclusion.
  [_step args]
  ;; Do not go one step forward, because a new premise / undercut has been added.
  (let [add-premise-args (dissoc args :present/premises :present/undercuts)
        ;; Get the id of the argument which can be undercut.
        undercut-id (database/argument-id-by-premise-conclusion (:premise/chosen args) (:conclusion/chosen args))]
    [[:premises/select args]
     [:support/new add-premise-args]
     [:rebut/new add-premise-args]
     [:undercut/new (assoc add-premise-args :undercut/argument-id undercut-id)]]))

(defmethod step :react-or-select-starting
  ;; The user can either select another premise for the current conclusion to discuss
  ;; or react with their own premise to the current conclusion.
  [_step args]
  (let [add-premise-args (dissoc args :present/premises)]
    [[:premises/select args]
     [:starting-support/new add-premise-args]
     [:starting-rebut/new add-premise-args]]))

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
  [_step {:keys [conclusion/chosen] :as args}]
  (let [arguments-to-select (database/all-arguments-for-conclusion (:db/id chosen))
        premises-to-select (build-meta-premises arguments-to-select)]
    ;; Add premises existing for chosen conclusion. Also keep the chosen conclusion
    ;; to properly create new attacks / supports.
    [:react-or-select-starting (-> args
                                   (dissoc :present/conclusions)
                                   (assoc :present/premises premises-to-select))]))

(defmethod react :starting-argument/new
  ;; User adds own starting argument. This is stored to the database and the
  ;; discussion flow is reset.
  [_step {:keys [discussion/id user/nickname new/starting-argument-conclusion new/starting-argument-premises]
          :as args}]
  (database/add-new-starting-argument! id nickname starting-argument-conclusion starting-argument-premises)
  [:discussion/start (dissoc args
                             :new/starting-argument-conclusion
                             :new/starting-argument-premises)])


(defmethod react :starting-support/new
  ;; The user has chosen to support the shown starting conclusion with their own premise.
  [_step {:keys [new/support-premise conclusion/chosen discussion/id user/nickname] :as args}]
  (let [new-argument-id (database/support-statement! id nickname chosen support-premise)]
    (database/set-argument-as-starting! id new-argument-id))
  [:react-or-select-starting (dissoc args :new/support-premise)])

(defmethod react :starting-rebut/new
  ;; The user has chosen to attack the shown conclusion with their own premise.
  [_step {:keys [new/rebut-premise conclusion/chosen discussion/id user/nickname] :as args}]
  (let [new-argument-id (database/attack-statement! id nickname chosen rebut-premise)]
    (database/set-argument-as-starting! id new-argument-id))
  [:react-or-select-starting (dissoc args :new/rebut-premise)])


;; -----------------------------------------------------------------------------
;; Selections
(defmethod react :premises/select
  ;; User has selected a premise to some argument. Show all premises that have the
  ;; selected premise as a conclusion. Also show all undercuts, which have a conclusion
  ;; which has the chosen premise as a premise.
  ;; A selected undercut has a normal statement as premise and the flow continues unabated.
  [_step {:keys [premise/chosen] :as args}]
  (let [arguments-to-select (database/all-arguments-for-conclusion (:db/id chosen))
        premises-to-select (build-meta-premises arguments-to-select)
        undercuts-to-select (map first (database/statements-undercutting-premise (:db/id chosen)))]
    [:react-or-select (-> args
                          (dissoc :present/conclusions)
                          (assoc :present/premises premises-to-select)
                          (assoc :present/undercuts undercuts-to-select))]))

;; -----------------------------------------------------------------------------
;; New Premises

(defmethod react :support/new
  ;; The user has chosen to support the shown conclusion with their own premise.
  [_step {:keys [new/support premise/chosen discussion/id user/nickname] :as args}]
  (database/support-statement! id nickname chosen support)
  [:react-or-select-after-addition (dissoc args :new/support)])

(defmethod react :rebut/new
  ;; The user has chosen to attack the shown conclusion with their own premise.
  [_step {:keys [new/rebut premise/chosen discussion/id user/nickname] :as args}]
  (database/attack-statement! id nickname chosen rebut)
  [:react-or-select-after-addition (dissoc args :new/rebut)])

(defmethod react :undercut/new
  ;; The user has chosen to attack the relation between the chosen premise and its previously
  ;; chosen conclusion. The argument-id of the argument that constitutes this is saved in
  ;; :undercut/argument-id of the map.
  [_step {:keys [new/undercut discussion/id user/nickname undercut/argument-id] :as args}]
  (database/undercut-argument! id nickname {:db/id argument-id} [undercut])
  [:react-or-select-after-addition (dissoc args :undercut/argument-id :new/undercut)])

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