(ns dialog.utils-test
  (:require [clojure.test :refer :all]
            [dialog.utils :as utils]
            [dialog.test.toolbelt :as test-toolbelt]))

(deftest map->nsmap-test
  (let [simple-map {:a 1}
        prefixed-members {:hullo/sir 1 :foo/bar 2 :peasant 3}
        no-keywords-here {"string-key" "hi" 2 "there General Kenobi"}
        keys-as-value {:key1 :key2}]
    (is (= {:prefix/a 1} (utils/map->nsmap simple-map "prefix")))
    (is (= {:prefix/a 1} (utils/map->nsmap simple-map :prefix)))
    (is (= {:hullo/sir 1 :foo/bar 2 :prefix/peasant 3}
           (utils/map->nsmap prefixed-members :prefix)))
    (is (= no-keywords-here (utils/map->nsmap no-keywords-here :prefix)))
    (is (= {:prefix/key1 :key2} (utils/map->nsmap keys-as-value :prefix)))))

(deftest ident-map->value-test
  (testing "No db/ident keys should be present anymore if the keys are correctly chosen."
    (are [x y] (= x y)
               {:foo :bar}
               (utils/ident-map->value {:foo {:db/ident :bar}} [:foo])
               {:foo :bar, :baz :oof}
               (utils/ident-map->value {:foo {:db/ident :bar}, :baz {:db/ident :oof}} [:foo :baz])))
  (testing "If the keys could not be found, there should still be the original map."
    (are [x y] (= x y)
               {:foo #:db{:ident :bar}}
               (utils/ident-map->value {:foo {:db/ident :bar}} [:non-existent])))
  (testing "Generative tests."
    (is (test-toolbelt/check? `utils/ident-map->value))))