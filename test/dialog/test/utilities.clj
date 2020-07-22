(ns dialog.test.utilities
  (:require [clojure.spec.test.alpha :as stest]
            [expound.alpha :as expound]
            [clojure.spec.alpha :as s]
            [dialog.discussion.database :as database]))

;; -----------------------------------------------------------------------------
;; Fixtures

(defn init-test-delete-db-fixture
  "Fixture to initialize, test, and afterwards delete the database."
  [f]
  (database/init-and-seed!)
  (f)
  (database/delete-database-from-config!))


;; -----------------------------------------------------------------------------
;; Generative Test Helpers

(defn- passed-all-tests?
  "`check` returns a list of tests. Get all results of these tests and check
  that they are all true."
  [results]
  (every? true?
          (map #(get-in % [:clojure.spec.test.check/ret :pass?]) results)))

(defn check?
  "Given a fully qualified function name, apply generative tests and pretty
  print the results, if there are any errors."
  [fn]
  (binding [s/*explain-out* expound/printer]
    (let [test-results (stest/check fn)]
      (if (passed-all-tests? test-results)
        true
        (do (expound/explain-results test-results) false)))))