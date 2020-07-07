(ns dialog.discussion.core
  (:require [dialog.discussion.database :as db]
            [clojure.spec.test.alpha :as spec-test]))

;; Used for properly starting the discussion service
(defn -main []
  (when-not (System/getenv "PRODUCTION")
    (spec-test/instrument))
  (db/init))

(comment
  (-main)
  :end)