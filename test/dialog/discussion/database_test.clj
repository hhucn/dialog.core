(ns dialog.discussion.database-test
  (:require [clojure.test :refer [deftest use-fixtures testing is]]
            [dialog.discussion.database :as database]
            [dialog.test.toolbelt :as test-toolbelt]))

(use-fixtures :each test-toolbelt/init-db-test-fixture)

(deftest all-discussions-test
  (testing "Return all discussions."
    (is (= 1 (count (database/all-discussions))))
    (is (test-toolbelt/check? `database/all-discussions))))

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
    (let [test-result (database/arguments-with-premise-content "dogs can act as watchdogs")
          presumed-result {:argument/version 1,
                           :argument/author #:author{:nickname "Wegi"},
                           :argument/type #:db{:ident :argument.type/support},
                           :argument/premises [{:statement/content "dogs can act as watchdogs",
                                                :statement/version 1,
                                                :statement/author #:author{:nickname "Wegi"}}],
                           :argument/conclusion {:statement/content "we should get a dog",
                                                 :statement/version 1,
                                                 :statement/author #:author{:nickname "Wegi"}}}]
      (is (= 1 (count test-result)))
      (is (= 1 (count (first test-result))))
      (is (= (-> presumed-result :argument/premises first :statement/content)
             (-> (ffirst test-result) :argument/premises first :statement/content)))
      (is (= (-> presumed-result :argument/conclusion :statement/content)
             (-> (ffirst test-result) :argument/conclusion :statement/content))))))

(deftest statements-undercutting-premise-test
  (testing "Given a original-premise deliver all premises undercutting arguments
  that have original-premise as a premise."
    (let [to-undercut (first (:argument/premises
                               (ffirst
                                 (database/arguments-with-premise-content "dogs can act as watchdogs"))))
          undercutting-statements (:statement/content (ffirst (database/statements-undercutting-premise (:db/id to-undercut))))]
      (is (or
            (= "Was hat eine Funktion mit einem Haustier zu tun?" undercutting-statements)
            (= "we have no use for a watchdog" undercutting-statements))))))
;; Note, the first statement in the or form comes from the engine test. The problem here
;; is the db not being emptied between tests.

(deftest argument-id-by-premise-conclusion-test
  (testing "See if the argument with corresponding premise and conclusion can be found"
    (is (nil? (database/argument-id-by-premise-conclusion 123 1234)))
    (let [cat-or-dog-id (:db/id (first (database/all-discussions-by-title "Cat or Dog?")))
          any-argument (first (filter
                                #(not= :argument.type/undercut (:argument/type %))
                                (database/all-arguments-for-discussion cat-or-dog-id)))
          premise (:db/id (first (:argument/premises any-argument)))
          conclusion (:db/id (:argument/conclusion any-argument))]
      (is (= (:db/id any-argument) (database/argument-id-by-premise-conclusion premise conclusion))))))

(deftest statements-by-content-test
  (testing "Statements are identified by identical content."
    (is (= 1 (count (database/statements-by-content "dogs can act as watchdogs"))))
    (is (= 1 (count (database/statements-by-content "we have no use for a watchdog"))))
    (is (empty? (database/statements-by-content "foo-baar-ajshdjkahsjdkljsadklja")))))

(deftest argument-id-by-undercut-and-premise-test
  (testing "See whether the right argument can be identified by providing undercuts premise and
  the premise of its conclusion."
    (let [undercut-id (:db/id (first (database/statements-by-content "we have no use for a watchdog")))
          conclusion-premise-id (:db/id (first (database/statements-by-content "dogs can act as watchdogs")))
          result (database/argument-id-by-undercut-and-premise undercut-id conclusion-premise-id)
          result-conclusion-id (:db/id (:argument/conclusion (database/fast-pull result)))
          result-con-con-id (:db/id (:argument/conclusion (database/fast-pull result-conclusion-id)))
          result-con-con (:statement/content (database/fast-pull result-con-con-id))]
      (is (= "we should get a dog" result-con-con)))))