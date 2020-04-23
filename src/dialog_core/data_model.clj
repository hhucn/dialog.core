(ns dialog-core.data-model
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
      ^DateTime created
      ^ID id]

     ^{:spec/tag-recursive true
       :datomic/tag-recursive true}
     Author
     [^String nickname
      ^ID id]

     ^{:union true
       :spec/tag-recursive true
       :datomic/tag-recursive true}
     EdgeTarget
     [Statement Edge]

     ^{:enum true
       :spec/tag-recursive true
       :datomic/tag-recursive true}
     EdgeType
     [SUPPORT ATTACK UNDERCUT]

     ^{:spec/tag-recursive true
       :datomic/tag-recursive true}
     Edge
     [^Author author
      ^Statement source
      ^EdgeType type
      ^EdgeTarget target
      ^DateTime created
      ^{:type Integer
        :optional true
        :default 1} version
      ^ID id]

     ^{:spec/tag-recursive true
       :datomic/tag-recursive true}
     Discussion
     [^String title
      ^String description
      ^{:type Edge
        :cardinality [0 n]} starting-edges
      ^ID id]]))

;; Hauptsächliche Fragen:
;; * Geht man den DBAS Weg mit Topics zu denen statements und argumente
;;   eindeutig zugewiesen sind?
;  * Geht man den EDEN weg in dem Statements und Argumente frei sind
;;   (siehe oben) Dann ist eine Diskussion nur eine Fragestellung mit
;;   argumenten die am Root hängen
;; * Braucht in dieser Art zu denken jedes objekt eine ID?


(def spec-schema (hodur-spec/schema core-data))
(comment spec-schema)
(def datomic-schema (hodur-datomic/schema core-data))
(comment datomic-schema)