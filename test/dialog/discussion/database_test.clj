(ns dialog.discussion.database-test
  (:require [clojure.test :refer :all]
            [dialog.discussion.database :as database]
            [dialog.test.utilities :as tutils]))

(use-fixtures :each tutils/init-test-delete-db-fixture)

(deftest all-arguments-for-discussion-test
  (testing "Should return valid arguments for valid discussion."
    (is (empty? (database/all-arguments-for-discussion 42)))))
