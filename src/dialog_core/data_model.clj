(ns dialog-core.data-model
  (:require [hodur-engine.core :as hodur]))

(def core-data (hodur/init-schema
                 '[Statement
                   [^Author author
                    ^String content
                    ^DateTime created
                    ^ID id]

                   Author
                   [^String nickname
                    ^ID id]

                   ^{:union true}
                   EdgeTarget
                   [Statement Edge]

                   ^{:enum true}
                   EdgeType
                   [SUPPORT ATTACK UNDERCUT]

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

                   Discussion
                   [^String title
                    ^String description
                    ^{:type Edge
                      :cardinality [0 n]} starting-edges
                    ^ID id]]))

;; Hauptsächliche Fragen:
;; * Geht man den DBAS Weg mit Topics zu denen statements und argumente
;;   eindeutig zugewiesen sind?
;  * Geht man den EDEN weg in denen Statements und Argumente frei sind
;;   (siehe oben) Dann ist eine Diskussion nur eine Fragestellung mit
;;   argumenten die am Root hängen
;; * Braucht in dieser Art zu denken jedes objekt eine ID?


