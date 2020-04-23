(ns dialog.core.models
  (:require [hodur-engine.core :as hodur]
            [hodur-spec-schema.core :as hodur-spec]
            [hodur-datomic-schema.core :as hodur-datomic]))

(def core-data
  (hodur/init-schema
   '[^{:spec/tag-recursive true
       :datomic/tag-recursive true}
     Statement
     [^Author author
      ^String content
      ^{:type Integer
        :default 1} version
      ^DateTime created
      ^DateTime modified
      ^ID id]

     ^{:spec/tag-recursive true
       :datomic/tag-recursive true}
     Author
     [^String nickname
      ^DateTime created
      ^DateTime modified
      ^ID id]

     ^{:union true
       :spec/tag-recursive true
       :datomic/tag-recursive true}
     ArgumentTarget
     [Statement Argument]

     ^{:enum true
       :spec/tag-recursive true
       :datomic/tag-recursive true}
     ArgumentType
     [SUPPORT ATTACK UNDERCUT]

     ^{:spec/tag-recursive true
       :datomic/tag-recursive true}
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

     ^{:spec/tag-recursive true
       :datomic/tag-recursive true}
     Discussion
     [^String title
      ^String description
      ^DateTime created
      ^DateTime modified
      ^{:type Argument
        :cardinality [0 n]} starting-arguments
      ^ID id]]))

(def spec-schema (hodur-spec/schema core-data))
(comment spec-schema)
(def datomic-schema (hodur-datomic/schema core-data))
(comment datomic-schema)