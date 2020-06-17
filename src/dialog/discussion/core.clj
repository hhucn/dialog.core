(ns dialog.discussion.core
  (:require [dialog.discussion.database :as db]))

;; Used for properly starting the discussion service
(defn -main []
  (db/init))