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
  [_step args]
  [[:arguments/starting args]])

(defmethod step :arguments/present
  [_step args]
  [[:argument/chosen args]])

(defmethod step :reactions/present
  [_step args]
  [[:reaction/support args]
   [:reaction/defend args]
   [:reaction/undermine args]
   [:reaction/undercut args]
   [:reaction/rebut args]])

(defmethod step :reasons/present
  [_step args]
  [[:reason/select args]
   [:reason/new args]])


;; -----------------------------------------------------------------------------

(defn find-attacking-argument [attitude arguments]
  ;; choose an attacking argument
  {})

(defn find-arguments-for-opinion [attitude arguments]
  {})

;; Das was der user auswählt
(defmulti react (fn [current-step reaction] reaction))

(defmethod react :arguments/present
  [_step {:keys [discussion/title is-start? user/attitude] :as args}]
  (let [arguments (if is-start?
                    (database/starting-arguments-by-title title)
                    (find-arguments-for-opinion attitude (database/arguments-by-title title)))]
    [[:arguments/present (merge args {:arguments/present arguments})]]))

(defmethod react :argument/chosen
  [_step args]
  ;; User has chosen an argument and the system is now attacking for it. This step
  ;; chooses the attacking argument.
  (let [attacking-argument (find-attacking-argument (:user/attitude args) (:argument/chosen args))]
    [[:reactions/present (merge args {:argument/attacking attacking-argument})]]))

(defmethod react :reaction/support
  [_step args]
  ;; User has chosen that they support the presented argument. Now, the system
  ;; looks for a new attacking argument.
  (let [attacking-argument (find-attacking-argument :pro (:argument/chosen args))]
    [[:reactions/present (merge args {:argument/attacking attacking-argument})]]))

(defmethod react :reaction/defend
  [_step args])
;; User accepts the presented argument, BUT still wants to defend their own
;; opinion.


(defmethod react :arguments/pro
  [_current-step argument]
  [[:reactions/present argument]])