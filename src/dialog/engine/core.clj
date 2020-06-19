(ns dialog.engine.core)

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

;; Das was der user auswählt
(defmulti react (fn [current-step reaction] reaction))

(defmethod react :arguments/pro
  [_current-step argument]
  [[:reactions/present argument]])