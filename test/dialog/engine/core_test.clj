(ns dialog.engine.core-test
  (:require [clojure.test :refer [deftest use-fixtures testing is]]
            [dialog.engine.core :as engine]))

(def ^:private two-test-arguments
  [{:db/id 92358976733280,
    :argument/version 1,
    :argument/author #:author{:nickname "Der Schredder"},
    :argument/type :argument.type/attack,
    :argument/premises [{:db/id 92358976733281,
                         :statement/content "you have to take the dog for a walk every day, which is tedious",
                         :statement/version 1,
                         :statement/author #:author{:nickname "Der Schredder"}}],
    :argument/conclusion {:db/id 92358976733275,
                          :statement/content "we should get a dog",
                          :statement/version 1,
                          :statement/author #:author{:nickname "Wegi"}}}
   {:db/id 123,
    :argument/version 1,
    :argument/author #:author{:nickname "Der Schredder"},
    :argument/type :argument.type/support,
    :argument/premises [{:db/id 1234,
                         :statement/content "test me baby one more time",
                         :statement/version 1,
                         :statement/author #:author{:nickname "Der Schredder"}}],
    :argument/conclusion {:db/id 12345,
                          :statement/content "this does not matter",
                          :statement/version 1,
                          :statement/author #:author{:nickname "Wegi"}}}])

(deftest meta-premise-builder-test
  (testing "Tests whether the function building the meta information into premises works."
    (is (= 2 (count (@#'engine/build-meta-premises two-test-arguments))))
    (is (= '({:db/id 92358976733281,
              :statement/content "you have to take the dog for a walk every day, which is tedious",
              :statement/version 1,
              :statement/author #:author{:nickname "Der Schredder"},
              :meta/argument.type :argument.type/attack}
             {:db/id 1234,
              :statement/content "test me baby one more time",
              :statement/version 1,
              :statement/author #:author{:nickname "Der Schredder"},
              :meta/argument.type :argument.type/support})
           (@#'engine/build-meta-premises two-test-arguments)))))
