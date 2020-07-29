(ns dialog.engine.core-test
  (:require [clojure.test :refer [deftest use-fixtures testing is]]
            [dialog.engine.core :as engine]
            [dialog.test.toolbelt :as test-toolbelt]
            [dialog.utils :as utils]
            [dialog.discussion.database :as database]))

(use-fixtures :each test-toolbelt/init-db-test-fixture)

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

(deftest premises-from-conclusion-id-test
  (testing "Find all conclusions given a certain conclusion-id."
    (let [starting-arguments (:discussion/starting-arguments (first (database/all-discussions-by-title "Cat or Dog?")))
          any-conclusion-id (:db/id (:argument/conclusion (first starting-arguments)))]
      (is (< 0 (count (@#'engine/premises-for-conclusion-id any-conclusion-id)))))))

(defn- step-with
  "Takes the `step` and runs `continue-discussion` with its args
  merged with `additionals`."
  [return step additionals]
  (let [args (utils/args-for-step return step)]
    (engine/continue-discussion step (merge args additionals))))

(defn- step-with-statement
  "Takes the `step` and runs `continue-discussion` with its args
  merged with `:statement/selected` with the value of `content` out of the `statement-map-key`."
  [return step statement-map-key content]
  (let [args (utils/args-for-step return step)
        statements (get args statement-map-key)
        selection (first (filter #(= content (:statement/content %)) statements))]
    (engine/continue-discussion step (merge args {:statement/selected selection}))))

(deftest continue-loop-test
  (testing "Tests whether the continue-loop is running without failures"
    (let [discussion-id (:db/id (first (database/all-discussions-by-title "Cat or Dog?")))
          start (engine/start-discussion {:user/nickname "Wegi"
                                          :discussion/id discussion-id})
          result (-> start
                     (step-with :starting-argument/new {:new/starting-argument-conclusion "Ich will eine Schildkröte."
                                                        :new/starting-argument-premises "Schildkrötenpanzer sind Hella stark."})
                     (step-with-statement :starting-conclusions/select :present/conclusions "we should get a dog")
                     (step-with :starting-support/new {:new/support-premise "Hunde sind einfach die knuffigsten"})
                     (step-with-statement :premises/select :present/premises "dogs can act as watchdogs")
                     (step-with :support/new {:new/support "Jaaa, Einbrecher haben tierische Angst vor Hunden"})
                     (step-with :rebut/new {:new/rebut "Viele Hunde sind dafür gar nicht geeignet"})
                     (step-with :undercut/new {:new/undercut "Was hat eine Funktion mit einem Haustier zu tun?"}))]
      (is (= 4 (count result)))
      (let [new-rebut-args (utils/args-for-step result :rebut/new)
            select-args (utils/args-for-step result :premises/select)]
        (is (= "we should get a dog" (get-in new-rebut-args [:conclusion/chosen :statement/content])))
        (is (= "dogs can act as watchdogs" (get-in new-rebut-args [:premise/chosen :statement/content])))
        (is (= 2 (count (:present/premises select-args))))
        (is (= 2 (count (:present/undercuts select-args))))))))