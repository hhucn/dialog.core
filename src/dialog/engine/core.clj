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

(>defn- premises-for-conclusion-id
  "Builds all meta-premises for a given conclusion."
  [conclusion-id]
  [number?
   :ret (s/coll-of ::models/statement)]
  (build-meta-premises (database/all-arguments-for-conclusion conclusion-id)))

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
        conclusions
        (distinct (map :argument/conclusion
                       (filter #(not= :argument.type/undercut (:argument/type %)) arguments)))]
    [[:starting-conclusions/select (merge args {:present/conclusions conclusions})]
     [:starting-argument/new (dissoc args :present/conclusions)]]))

(defmethod step :react-or-select
  ;; The user can either select another premise for the current conclusion to discuss
  ;; or react with their own premise to the current conclusion.
  ;; Also show all undercuts, which undercut an argument with the chosen premise as a premise.
  [_step {:keys [premise/chosen statement/selected] :as args}]
  ;; First go one step further, by setting the last premise as the next conclusion
  ;; This way the next chosen premise is still correctly matching it.
  ;; If premise/chosen has never been set it needs to be done once.
  (let [premises-to-select (premises-for-conclusion-id (:db/id selected))
        undercuts-to-select (map first (database/statements-undercutting-premise (:db/id selected)))
        raw-react-select-args (assoc (dissoc args :statement/selected)
                                :premise/chosen selected
                                :present/undercuts undercuts-to-select
                                :present/premises premises-to-select)
        ;; Rotate premise back if it exists, otherwise just set the selected as premise and leave conclusion
        select-args (if chosen
                      (assoc raw-react-select-args :conclusion/chosen chosen)
                      raw-react-select-args)
        ;; Only rotate premise and conclusion in case of a new selected premise.
        add-premise-args (dissoc select-args :present/premises :present/undercuts)
        ;; Get the id of the argument which can be undercut.
        undercut-id (database/argument-id-by-premise-conclusion
                      (:db/id (or chosen selected))
                      (:db/id (:conclusion/chosen args)))]
    [[:premises/select select-args]
     [:support/new add-premise-args]
     [:rebut/new add-premise-args]
     [:undercut/new (assoc add-premise-args :undercut/argument-id undercut-id)]]))

(defmethod step :react-or-select-after-addition
  ;; The user can either select another premise for the current conclusion to discuss
  ;; or react with their own premise to the current conclusion.
  [_step {:keys [premise/chosen] :as args}]
  ;; Do not go one step forward, because a new premise / undercut has been added.
  (let [premises-to-select (premises-for-conclusion-id (:db/id chosen))
        undercuts-to-select (map first (database/statements-undercutting-premise (:db/id chosen)))
        select-args (assoc args
                      :present/premises premises-to-select
                      :present/undercuts undercuts-to-select)
        add-premise-args (dissoc args :present/premises :present/undercuts)
        ;; Get the id of the argument which can be undercut.
        undercut-id (database/argument-id-by-premise-conclusion
                      (:db/id (:premise/chosen args))
                      (:db/id (:conclusion/chosen args)))]
    [[:premises/select select-args]
     [:support/new add-premise-args]
     [:rebut/new add-premise-args]
     [:undercut/new (assoc add-premise-args :undercut/argument-id undercut-id)]]))

(defmethod step :react-or-select-starting
  ;; The user can either select another premise for the current conclusion to discuss
  ;; or react with their own premise to the current conclusion.
  [_step {:keys [statement/selected conclusion/chosen] :as args}]
  (let [chosen (or chosen selected)
        ;; In the case the user comes from a fresh start, selected is set as the chosen conclusion
        premises-to-select (premises-for-conclusion-id (:db/id chosen))
        temp-args (assoc (dissoc args :statement/selected) :conclusion/chosen chosen)
        select-args (assoc temp-args :present/premises premises-to-select)
        add-premise-args (dissoc temp-args :present/premises)]
    [[:premises/select select-args]
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
  [_step {:keys [statement/selected] :as args}]
  (let [premises-to-select (premises-for-conclusion-id (:db/id selected))]
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
  (database/add-new-starting-argument! id nickname starting-argument-conclusion [starting-argument-premises])
  [:discussion/start (dissoc args
                             :new/starting-argument-conclusion
                             :new/starting-argument-premises)])


(defmethod react :starting-support/new
  ;; The user has chosen to support the shown starting conclusion with their own premise.
  [_step {:keys [new/support-premise conclusion/chosen discussion/id user/nickname] :as args}]
  (let [new-argument-id (database/support-statement! id nickname chosen support-premise)]
    (database/set-argument-as-starting! id new-argument-id)
    [:react-or-select-starting (dissoc args :new/support-premise)]))

(defmethod react :starting-rebut/new
  ;; The user has chosen to attack the shown conclusion with their own premise.
  [_step {:keys [new/rebut-premise conclusion/chosen discussion/id user/nickname] :as args}]
  (let [new-argument-id (database/attack-statement! id nickname chosen rebut-premise)]
    (database/set-argument-as-starting! id new-argument-id)
    [:react-or-select-starting (dissoc args :new/rebut-premise)]))


;; -----------------------------------------------------------------------------
;; Selections
(defmethod react :premises/select
  ;; User has selected a premise to some argument.
  ;; A selected undercut has a normal statement as premise and the flow continues unabated.
  [_step args]
  [:react-or-select (dissoc args :present/conclusions)])

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
  (:db/id (first (database/all-discussions-by-title "Cat or Dog?")))
  (start-discussion {:user/nickname "Wegi"
                     :discussion/id 96757023244429})
  (continue-discussion
    :starting-argument/new
    {:user/nickname "Wegi", :discussion/id 96757023244429
     ;; gesetzt
     :new/starting-argument-conclusion "Ich will eine Schildkröte."
     :new/starting-argument-premises "Schildkrötenpanzer sind Hella stark."})
  (continue-discussion
    :starting-conclusions/select
    {:user/nickname "Wegi",
     :discussion/id 96757023244429,
     :present/conclusions '({:db/id 83562883711120,
                             :statement/content "Ich will eine Schildkröte.",
                             :statement/version 1,
                             :statement/author #:author{:nickname "Wegi"}}
                            {:db/id 96757023244379,
                             :statement/content "we should get a dog",
                             :statement/version 1,
                             :statement/author #:author{:nickname "Wegi"}}
                            {:db/id 96757023244381,
                             :statement/content "we could get both, a dog and a cat",
                             :statement/version 1,
                             :statement/author #:author{:nickname "Christian"}}
                            {:db/id 96757023244380,
                             :statement/content "we should get a cat",
                             :statement/version 1,
                             :statement/author #:author{:nickname "Der Schredder"}})
     ;; gesetzt
     :statement/selected {:db/id 96757023244379,
                          :statement/content "we should get a dog",
                          :statement/version 1,
                          :statement/author #:author{:nickname "Wegi"}}})
  (continue-discussion
    :starting-support/new
    {:user/nickname "Wegi",
     :discussion/id 96757023244429,
     :conclusion/chosen {:db/id 96757023244379,
                         :statement/content "we should get a dog",
                         :statement/version 1,
                         :statement/author #:author{:nickname "Wegi"}}
     ;; Selbst gesetzt
     :new/support-premise "Hunde sind einfach die knuffigsten"})
  (continue-discussion
    :premises/select
    {:user/nickname "Wegi",
     :discussion/id 96757023244429,
     :conclusion/chosen {:db/id 96757023244379,
                         :statement/content "we should get a dog",
                         :statement/version 1,
                         :statement/author #:author{:nickname "Wegi"}},
     :present/premises '({:db/id 96757023244383,
                          :statement/content "dogs can act as watchdogs",
                          :statement/version 1,
                          :statement/author #:author{:nickname "Wegi"},
                          :meta/argument.type :argument.type/support}
                         {:db/id 96757023244385,
                          :statement/content "you have to take the dog for a walk every day, which is tedious",
                          :statement/version 1,
                          :statement/author #:author{:nickname "Der Schredder"},
                          :meta/argument.type :argument.type/attack}
                         {:db/id 96757023244433,
                          :statement/content "Hunde sind einfach die knuffigsten",
                          :statement/version 1,
                          :statement/author #:author{:nickname "Wegi"},
                          :meta/argument.type :argument.type/support})
     ;; Selbst gesetzt
     :statement/selected {:db/id 96757023244383,
                          :statement/content "dogs can act as watchdogs",
                          :statement/version 1,
                          :statement/author #:author{:nickname "Wegi"},
                          :meta/argument.type :argument.type/support}})
  (continue-discussion
    :support/new
    {:user/nickname "Wegi",
     :discussion/id 96757023244429,
     :conclusion/chosen {:db/id 96757023244379,
                         :statement/content "we should get a dog",
                         :statement/version 1,
                         :statement/author #:author{:nickname "Wegi"}},
     :premise/chosen {:db/id 96757023244383,
                      :statement/content "dogs can act as watchdogs",
                      :statement/version 1,
                      :statement/author #:author{:nickname "Wegi"},
                      :meta/argument.type :argument.type/support}
     ;; Selbst gesetzt
     :new/support "Jaaa, Einbrecher haben tierische Angst vor Hunden"})
  (continue-discussion
    :rebut/new
    {:user/nickname "Wegi",
     :discussion/id 96757023244429,
     :conclusion/chosen {:db/id 96757023244379,
                         :statement/content "we should get a dog",
                         :statement/version 1,
                         :statement/author #:author{:nickname "Wegi"}},
     :premise/chosen {:db/id 96757023244383,
                      :statement/content "dogs can act as watchdogs",
                      :statement/version 1,
                      :statement/author #:author{:nickname "Wegi"},
                      :meta/argument.type :argument.type/support}
     ;; gesetzt
     :new/rebut "Viele Hunde sind dafür gar nicht geeignet"})
  (continue-discussion
    :undercut/new
    {:user/nickname "Wegi",
     :discussion/id 96757023244429,
     :conclusion/chosen {:db/id 96757023244379,
                         :statement/content "we should get a dog",
                         :statement/version 1,
                         :statement/author #:author{:nickname "Wegi"}},
     :premise/chosen {:db/id 96757023244383,
                      :statement/content "dogs can act as watchdogs",
                      :statement/version 1,
                      :statement/author #:author{:nickname "Wegi"},
                      :meta/argument.type :argument.type/support},
     :undercut/argument-id 96757023244382
     ;; gesetzt
     :new/undercut "Was hat eine Funktion mit einem Haustier zu tun?"})
  :end)