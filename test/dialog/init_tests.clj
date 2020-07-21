(ns dialog.init-tests
  "Called before the tests are run."
  (:require [taoensso.timbre :refer [debug]]
            [dialog.discussion.core :as discussion-main]))

(defn -main []
  (debug "Initializing and seeding the database.")
  (discussion-main/-main))

(-main)