(ns dialog.discussion.config
  (:require [dialog.utils :as utils]))

;; Dev config. Need a proper way to handle switch when in production.
;; ##################################################################
(def datomic
  {:system "development"
   :server-type :dev-local
   :storage-dir (utils/create-storage-directory!)})

(def db-name (or (System/getenv "DATOMIC_DISCUSSION_DB_NAME") "dev-db"))