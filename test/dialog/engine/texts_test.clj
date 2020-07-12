(ns dialog.engine.texts-test
  (:require
    [clojure.string :as string]
    [clojure.test :refer [deftest is are testing]]
    [dialog.engine.texts :as texts]
    [dialog.test.utilities :as tutils]))

(deftest argument-test
  (testing "Function should always return a format string."
    (is (string/includes? (texts/argument) "%s"))))

(deftest concat-premises-test
  (testing "Generative tests when concatenating premises."
    (is (tutils/check? `texts/concat-premises))))
