(ns dialog.utils-test
  (:require [clojure.test :refer :all]
            [dialog.utils :as utils]))

(deftest test-map->nsmap
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