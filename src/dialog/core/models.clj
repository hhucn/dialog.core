(ns dialog.core.models
  (:require [hodur-engine.core :as hodur]
            [hodur-spec-schema.core :as hodur-spec]
            [hodur-datomic-schema.core :as hodur-datomic]))

(def core-data
  (hodur/init-schema
    '[^{:spec/tag true
        :datomic/tag true}
      default

      Statement
      [^Author author
       ^String content
       ^{:type Integer
         :default 1} version
       ^DateTime created
       ^DateTime modified]

      Author
      [^String nickname
       ^DateTime created
       ^DateTime modified]

      ^{:union true}
      ArgumentTarget
      [Statement Argument]

      ^{:enum true}
      ArgumentType
      [SUPPORT ATTACK UNDERCUT]

      Argument
      [^Author author
       ^{:type Statement
         :cardinality [0 n]} premises
       ^ArgumentType type
       ^ArgumentTarget conclusion
       ^DateTime created
       ^DateTime modified
       ^{:type Integer
         :default 1} version]

      Discussion
      [^String title
       ^String description
       ^DateTime created
       ^DateTime modified
       ^{:type Argument
         :cardinality [0 n]} starting-arguments
       ^Boolean closed]]))

(def spec-schema (hodur-spec/schema core-data))
;; This defspecs needs to be run to have the specs in this namespace.
(hodur-spec/defspecs core-data)
(comment spec-schema)
;; The following will define all specs with the prefix foo instead of this namespace.
;; We can use this in tests to generate correct specs. There seems to be no option to cut the namespace
;; alltogether. one could annotate aliases for every attribute, but that is as tedious as writing
;; separate specs and datomic.
(comment (hodur-spec/defspecs core-data {:prefix :foo}))

(def datomic-schema (hodur-datomic/schema core-data))
(comment datomic-schema)