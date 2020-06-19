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
  [_step {:keys [discussion/title]}]
  ;; Suche alle starting-arguments zu title
  {:arguments/starting []})

(defmethod step :arguments/present
  [_step {:keys [discussion/title argument/is-starting?]}]
  [[:arguments/pro title is-starting?]
   [:arguments/con title is-starting?]])

(defmethod step :reactions/present
  [_step argument]
  [[:reaction/support argument]
   [:reaction/defend argument]
   [:reaction/undermine argument]
   [:reaction/undercut argument]
   [:reaction/rebut argument]])

(defmethod step :reasons/present
  [_step {:keys [argument attack/type]}]
  [[:reason/select argument type]
   [:reason/new argument type]])


;; -----------------------------------------------------------------------------

;; Das was der user auswählt
(defmulti react (fn [current-step reaction] reaction))

(defmethod react :arguments/pro
  [_current-step argument]
  [[:reactions/present argument]])