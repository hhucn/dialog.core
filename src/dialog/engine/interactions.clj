(ns dialog.engine.interactions
  (:require [clojure.spec.alpha :as s]
            [dialog.engine.texts :as texts]))

(defn- confirmed? []
  (println "Are you satisfied with your input? [y/n]")
  (let [input (.toLowerCase (read-line))]
    (= "y" input)))

(s/fdef confirmed? :ret boolean?)

(defn ask-for-new-input
  "Generic function to ask for user input."
  [formatted-statements introduction input-request]
  (println introduction "\n")
  (println formatted-statements "\n")
  (println input-request)
  (let [new-statement (read-line)]
    (when (confirmed?)
      (println "🎉 You entered" new-statement)
      new-statement)))

(s/fdef ask-for-new-input
        :args (s/cat :formatted-statements string? :introduction string?
                     :input-request string?)
        :ret string?)

(defn ask-for-new-support [argument]
  (ask-for-new-input
    (texts/format-premises (:argument/premises argument))
    "Please define why you want to support the following statement:"
    "Now, how do you want to support this statement?"))

(s/fdef ask-for-new-support
        :args (s/cat :argument (s/keys :req [:argument/premises]))
        :ret string?)

(defn ask-for-new-defend [argument]
  (ask-for-new-input
    (texts/format-statement (:argument/conclusion argument))
    "Please define why you want to support the following statement:"
    "Now, how do you want to support this statement?"))

(s/fdef ask-for-new-support
        :args (s/cat :argument (s/keys :req [:argument/premises]))
        :ret string?)

(defn ask-for-new-attack [formatted-statement]
  (ask-for-new-input
    formatted-statement
    "Please define why you want to attack the following statement:"
    "Now, how to you want to attack this statement?"))

(s/fdef ask-for-new-attack
        :args (s/cat :formatted-statement string?)
        :ret string?)