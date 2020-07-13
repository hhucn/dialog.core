(ns dialog.engine.texts-test
  (:require
    [clojure.string :as string]
    [clojure.test :refer [deftest is are testing]]
    [dialog.engine.texts :as texts]
    [dialog.test.utilities :as tutils]))

(deftest argument-with-author-test
  (testing "Function should always return a format string."
    (is (string/includes? (texts/argument-with-author) "%s"))))

(deftest concat-premises-test
  (testing "Generative tests when concatenating premises."
    (is (tutils/check? `texts/concat-premises))))

(deftest avatar-with-nickname-test
  (testing "Nickname should still remain in the resulting string."
    (is (tutils/check? `texts/avatar-with-nickname))
    (let [nickname "kangaroo"]
      (is (string/includes? (texts/avatar-with-nickname nickname) nickname)))))