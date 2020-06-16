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
       ^DateTime modified
       ^ID id]

      Author
      [^String nickname
       ^DateTime created
       ^DateTime modified
       ^ID id]

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
         :default 1} version
       ^ID id]

      Discussion
      [^String title
       ^String description
       ^DateTime created
       ^DateTime modified
       ^{:type Argument
         :cardinality [0 n]} starting-arguments
       ^ID id
       ^Boolean closed]]))

(def spec-schema (hodur-spec/schema core-data))
(comment spec-schema)
(def datomic-schema (hodur-datomic/schema core-data))
(comment datomic-schema)