(ns dialog.cli.texts-test
  (:require
    [clojure.test :refer [deftest is testing]]
    [clojure.string :as string]
    [dialog.cli.texts :as texts]
    [dialog.test.utilities :as tutils]))

(def argument-attack
  {:db/id 17592186045489,
   :argument/version 1,
   :argument/author #:author{:nickname "Christian"},
   :argument/type :argument.type/attack,
   :argument/premises [{:db/id 17592186045490,
                        :statement/content "i do not like dogs",
                        :statement/version 1,
                        :statement/author #:author{:nickname "Christian"}}],
   :argument/conclusion {:db/id 17592186045429,
                         :statement/content "we should get a dog",
                         :statement/version 1,
                         :statement/author #:author{:nickname "Wegi"}}})

(def argument-undercut
  {:db/id 17592186045469,
   :argument/version 1,
   :argument/author #:author{:nickname "Der Schredder"},
   :argument/type :argument.type/undercut,
   :argument/premises [{:db/id 17592186045470,
                        :statement/content "this is based on the cats race and on the breeding, and is not inherent for cats.",
                        :statement/version 1,
                        :statement/author #:author{:nickname "Der Schredder"}}],
   :argument/conclusion {:db/id 17592186045467,
                         :argument/version 1,
                         :argument/author #:author{:nickname "Wegi"},
                         :argument/type :argument.type/attack,
                         :argument/premises [{:db/id 17592186045468,
                                              :statement/content "cats are capricious",
                                              :statement/version 1,
                                              :statement/author #:author{:nickname "Wegi"}}],
                         :argument/conclusion {:db/id 17592186045430,
                                               :statement/content "we should get a cat",
                                               :statement/version 1,
                                               :statement/author #:author{:nickname "Der Schredder"}}}})

(deftest argument-with-author-test
  (testing "Function should always return a format string."
    (is (string/includes? (texts/argument-with-author "kangaroo") "%s"))))

(deftest concat-premises-test
  (testing "Generative tests when concatenating premises."
    (is (tutils/check? `texts/concat-premises))))

(deftest avatar-with-nickname-test
  (testing "Nickname should still remain in the resulting string."
    (is (tutils/check? `texts/avatar-with-nickname))
    (let [nickname "kangaroo"]
      (is (string/includes? (texts/avatar-with-nickname nickname) nickname)))))

(deftest format-argument-test
  (testing "Arguments are being processed"
    (is (string? (texts/format-argument argument-attack)))
    (is (string? (texts/format-argument argument-undercut)))))
