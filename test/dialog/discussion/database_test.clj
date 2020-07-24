(ns dialog.discussion.database-test
  (:require [clojure.test :refer [deftest use-fixtures testing is]]
            [dialog.discussion.database :as database]
            [dialog.test.toolbelt :as test-toolbelt]))

(use-fixtures :each test-toolbelt/init-db-test-fixture)

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

(deftest arguments-with-premise-content-test
  (testing "Should find all arguments containing a premise with certain content."
    (let [test-result (database/arguments-with-premise-content "dogs can act as watchdogs")]
      (is (= 1 (count test-result)))
      (is (= 1 (count (first test-result))))
      (is (= {:db/id 92358976733278,
              :argument/version 1,
              :argument/author #:author{:nickname "Wegi"},
              :argument/type #:db{:ident :argument.type/support},
              :argument/premises [{:db/id 92358976733279,
                                   :statement/content "dogs can act as watchdogs",
                                   :statement/version 1,
                                   :statement/author #:author{:nickname "Wegi"}}],
              :argument/conclusion {:db/id 92358976733275,
                                    :statement/content "we should get a dog",
                                    :statement/version 1,
                                    :statement/author #:author{:nickname "Wegi"}}}
             (ffirst test-result))))))

(deftest statements-undercutting-premise-test
  (testing "Given a original-premise deliver all premises undeructting arguments that have original-premise
  as a premise"
    (let [desired-result (first (:argument/premises
                                  (database/arguments-with-premise-content "dogs can act as watchdogs")))]
      (is (= desired-result (database/statements-undercutting-premise (:db/id desired-result)))))))