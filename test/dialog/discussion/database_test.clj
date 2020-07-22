(ns dialog.discussion.database-test
  (:require [clojure.test :refer :all]
            [dialog.discussion.database :as database]
            [dialog.test.utilities :as tutils]))

(use-fixtures :each tutils/init-test-delete-db-fixture)

(deftest all-arguments-for-discussion-test
  (testing "Should return valid arguments for valid discussion."
    (let [cat-or-dog-id (:db/id (first (database/all-discussions-by-title "Cat or Dog?")))]
      (is (empty? (database/all-arguments-for-discussion -1)))
      (is (seq (database/all-arguments-for-discussion cat-or-dog-id))))))