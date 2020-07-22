(ns dialog.discussion.core
  (:require [dialog.discussion.database :as db]
            [clojure.spec.test.alpha :as spec-test]
            [dialog.discussion.config :as config]))

;; Used for properly starting the discussion service
(defn -main []
  (when-not (System/getenv "PRODUCTION")
    (spec-test/instrument))
  ;; This initializes the db with standard in memory data when run on its own.
  (db/init-and-seed! {:datomic config/datomic
                      :name config/db-name}))

(comment
  (-main)
  :end)