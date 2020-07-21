(ns dialog.init-tests
  "Called before the tests are run."
  (:require [dialog.discussion.core :as dbcore]))

(dbcore/-main)