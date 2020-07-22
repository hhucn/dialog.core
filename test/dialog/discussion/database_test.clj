(ns dialog.discussion.database-test
  (:require [clojure.test :refer [deftest use-fixtures testing is]]
            [dialog.discussion.database :as database]
            [dialog.test.utilities :as tutils]))

(use-fixtures :each tutils/init-test-delete-db-fixture)

(deftest all-discussions-by-title-test
  (testing "Should return discussions if title matches at least one discussion."
    (is (empty? (database/all-discussions-by-title "")))
    (is (empty? (database/all-discussions-by-title "👾")))
    (is (seq (database/all-discussions-by-title "Cat or Dog?")))))

(deftest all-arguments-for-discussion-test
  (testing "Should return valid arguments for valid discussion."
    (let [cat-or-dog-id (:db/id (first (database/all-discussions-by-title "Cat or Dog?")))]
      (is (empty? (database/all-arguments-for-discussion -1)))
      (is (seq (database/all-arguments-for-discussion cat-or-dog-id))))))
