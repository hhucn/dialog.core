(ns dialog.discussion.administration-test
  (:require [clojure.test :refer :all]
            [dialog.discussion.administration :as discussion]))

(defn- duplicate-discussion-states?
  "Tests whether two discussion states are duplicate."
  [discussion]
  (not= (count (:state discussion))
        (count (set (:state discussion)))))

(defn- conflicting-discussion-states?
  "Tests whether two discussion states are conflicting."
  [discussion]
  (let [states (set (:state discussion))]
    (and (contains? states :open)
         (contains? states :closed))))

(deftest test-empty-discussion
  (let [new-discussion (discussion/empty-discussion "test" "some description"
                                                    {:extra-opt "hi"})]
    (is (not (duplicate-discussion-states? new-discussion)))
    (is (not (conflicting-discussion-states? new-discussion)))
    (is (= "hi" (:extra-opt new-discussion)))))