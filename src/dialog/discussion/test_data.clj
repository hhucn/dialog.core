(ns dialog.discussion.test-data)

(def ^:private cat-or-dog-authors
  [{:db/id "author/wegi" :author/nickname "Wegi"}
   {:db/id "author/tobi" :author/nickname "Tobias"}
   {:db/id "author/rambo" :author/nickname "Christian"}
   {:db/id "author/stinky" :author/nickname "Der miese Peter"}])

(def ^:private cat-or-dog-statements
  [{:db/id "statement/get-dog"
    :statement/author "author/wegi"                         ; Use the tempid above
    :statement/content "we should get a dog"
    :statement/version 1}
   {:db/id "statement/get-cat"
    :statement/author "author/tobi"                         ; Use the tempid above
    :statement/content "we should get a cat"
    :statement/version 1}
   {:db/id "statement/get-both"
    :statement/author "author/rambo"                        ; Use the tempid above
    :statement/content "we could get both, a dog and a cat"
    :statement/version 1}])

(def ^:private cat-or-dog-arguments
  [{:db/id "argument/watchdogs"
    :argument/author "author/wegi"
    :argument/premises [{:db/id "statement/watchdogs"
                         :statement/author "author/wegi"    ; Use the tempid above
                         :statement/content "dogs can act as watchdogs"
                         :statement/version 1}]
    :argument/conclusion "statement/get-dog"
    :argument/version 1
    :argument/type :argument.type/support}
   {:db/id "argument/tedious-dogs"
    :argument/author "author/tobi"
    :argument/premises [{:db/id "statement/walks"
                         :statement/author "author/tobi"    ; Use the tempid above
                         :statement/content "you have to take the dog for a walk every
                         day, which is tedious"
                         :statement/version 1}]
    :argument/conclusion "statement/get-dog"
    :argument/version 1
    :argument/type :argument.type/attack}
   {:argument/author "author/stinky"
    :argument/premises [{:db/id "statement/no-use"
                         :statement/author "author/stinky"
                         :statement/content "we have no use for a watchdog"
                         :statement/version 1}]
    :argument/conclusion "argument/watchdogs"
    :argument/version 1
    :argument/type :argument.type/undercut}
   {:argument/author "author/stinky"
    :argument/premises [{:db/id "statement/exercise"
                         :statement/author "author/stinky"
                         :statement/content "going for a walk with the dog every day
                         is good for social interaction and physical exercise"
                         :statement/version 1}]
    :argument/conclusion "argument/tedious-dogs"
    :argument/version 1
    :argument/type :argument.type/undercut}
   {:db/id "argument/both-is-fine"
    :argument/author "author/rambo"
    :argument/premises [{:db/id "statement/no-problem"
                         :statement/author "author/rambo"
                         :statement/content "it would be no problem"
                         :statement/version 1}]
    :argument/conclusion "statement/get-both"
    :argument/version 1
    :argument/type :argument.type/support}
   {:argument/author "author/wegi"
    :argument/premises [{:db/id "statement/moneeey"
                         :statement/author "author/wegi"
                         :statement/content "we do not have enough money for two pets"
                         :statement/version 1}]
    :argument/conclusion "statement/no-problem"
    :argument/version 1
    :argument/type :argument.type/attack}
   ;; Here be premise groups
   {:db/id "argument/hate"
    :argument/author "author/stinky"
    :argument/premises [{:db/id "statement/best-friends"
                         :statement/author "author/stinky"
                         :statement/content "won't be best friends"
                         :statement/version 1}
                        {:db/id "statement/strong-hate"
                         :statement/author "author/stinky"
                         :statement/content "a cat and a dog will generally not get
                         along well"
                         :statement/version 1}]
    :argument/conclusion "argument/both-is-fine"
    :argument/version 1
    :argument/type :argument.type/undercut}
   {:db/id "argument/independent-cats"
    :argument/author "author/tobi"
    :argument/premises [{:db/id "statement/independent"
                         :statement/author "author/tobi"
                         :statement/content "cats are very independent"
                         :statement/version 1}]
    :argument/conclusion "statement/get-cat"
    :argument/version 1
    :argument/type :argument.type/support}
   {:argument/author "author/wegi"
    :argument/premises [{:db/id "statement/take-care-baby"
                         :statement/author "author/wegi"
                         :statement/content "the purpose of a pet is to have something
                         to take care of"
                         :statement/version 1}]
    :argument/conclusion "argument/independent-cats"
    :argument/version 1
    :argument/type :argument.type/undercut}
   {:argument/author "author/wegi"
    :argument/premises [{:db/id "statement/overbred"
                         :statement/author "author/wegi"
                         :statement/content "this is not true for overbred races"
                         :statement/version 1}]
    :argument/conclusion "statement/independent"
    :argument/version 1
    :argument/type :argument.type/attack}
   {:argument/author "author/tobi"
    :argument/premises [{:db/id "statement/darwin-likes"
                         :statement/author "author/tobi"
                         :statement/content "this lies in their natural conditions"
                         :statement/version 1}]
    :argument/conclusion "statement/independent"
    :argument/version 1
    :argument/type :argument.type/support}
   {:db/id "argument/hunters"
    :argument/author "author/rambo"
    :argument/premises [{:db/id "statement/ancestry"
                         :statement/author "author/rambo"
                         :statement/content "cats ancestors are animals in wildlife,
                          who are hunting alone and not in groups"
                         :statement/version 1}]
    :argument/conclusion "statement/independent"
    :argument/version 1
    :argument/type :argument.type/support}
   {:argument/author "author/wegi"
    :argument/premises [{:db/id "statement/wild-thang"
                         :statement/author "author/wegi"
                         :statement/content "house cats are not wild cats anymore"
                         :statement/version 1}]
    :argument/conclusion "argument/hunters"
    :argument/version 1
    :argument/type :argument.type/undercut}
   {:db/id "argument/no-taxes"
    :argument/author "author/tobi"
    :argument/premises [{:db/id "statement/taxes"
                         :statement/author "author/tobi"
                         :statement/content "a cat does not cost taxes like a dog does"
                         :statement/version 1}]
    :argument/conclusion "statement/get-cat"
    :argument/version 1
    :argument/type :argument.type/support}
   {:argument/author "author/stinky"
    :argument/premises [{:db/id "statement/credibility"
                         :statement/author "author/stinky"
                         :statement/content "thats what you just say without a credible
                         source"
                         :statement/version 1}]
    :argument/conclusion "argument/no-taxes"
    :argument/version 1
    :argument/type :argument.type/undercut}
   {:argument/author "author/rambo"
    :argument/premises [{:db/id "statement/germoney"
                         :statement/author "author/rambo"
                         :statement/content "in germany a second dog costs even
                         more taxes"
                         :statement/version 1}]
    :argument/conclusion "statement/taxes"
    :argument/version 1
    :argument/type :argument.type/support}
   {:argument/author "author/rambo"
    :argument/premises [{:db/id "statement/doggo-same"
                         :statement/author "author/rambo"
                         :statement/content "other costs of living for cats and
                         dogs are nearly the same"
                         :statement/version 1}]
    :argument/conclusion "statement/taxes"
    :argument/version 1
    :argument/type :argument.type/support}
   {:db/id "argument/moody-cats"
    :argument/author "author/wegi"
    :argument/premises [{:db/id "statement/moody"
                         :statement/author "author/wegi"
                         :statement/content "cats are capricious"
                         :statement/version 1}]
    :argument/conclusion "statement/get-cat"
    :argument/version 1
    :argument/type :argument.type/attack}
   {:argument/author "author/tobi"
    :argument/premises [{:db/id "statement/race-dogs"
                         :statement/author "author/tobi"
                         :statement/content "this is based on the cats race and
                          on the breeding, and is not inherent for cats."
                         :statement/version 1}]
    :argument/conclusion "argument/moody-cats"
    :argument/version 1
    :argument/type :argument.type/undercut}
   {:argument/author "author/tobi"
    :argument/premises [{:db/id "statement/catcatcatcat"
                         :statement/author "author/tobi"
                         :statement/content "the fact, that cats are capricious,
                         is based on the cats race"
                         :statement/version 1}]
    :argument/conclusion "statement/moody"
    :argument/version 1
    :argument/type :argument.type/attack}
   {:argument/author "author/tobi"
    :argument/premises [{:db/id "statement/not-all-cats"
                         :statement/author "author/tobi"
                         :statement/content "not every cat is capricious"
                         :statement/version 1}]
    :argument/conclusion "statement/moody"
    :argument/version 1
    :argument/type :argument.type/attack}
   {:argument/author "author/rambo"
    :argument/premises [{:db/id "statement/fire-cats"
                         :statement/author "author/rambo"
                         :statement/content "several cats of my friends are real
                         assholes"
                         :statement/version 1}]
    :argument/conclusion "statement/moody"
    :argument/version 1
    :argument/type :argument.type/support}])

(def ^:private cat-or-dog-discussion
  [{:discussion/title "Cat or Dog?"
    :discussion/description "Should a person looking for a pet rather buy a dog or
    a cat?"
    :discussion/states [:discussion.state/open]
    :discussion/starting-arguments ["argument/watchdogs" "argument/tedious-dogs"
                                    "argument/both-is-fine" "argument/hate"
                                    "argument/independent-cats" "argument/no-taxes"
                                    "argument/moody-cats"]}])

(def testdata-cat-or-dog
  (concat cat-or-dog-authors cat-or-dog-statements cat-or-dog-arguments
          cat-or-dog-discussion))