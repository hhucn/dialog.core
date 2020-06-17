(ns dialog.discussion.config)

;; Dev config. Need a proper way to handle switch when in production.
;; ##################################################################
(def datomic
  (let [host (or (System/getenv "DATOMIC_HOST") "localhost")
        port (or (System/getenv "DATOMIC_PORT") "8998")]
    {:server-type :peer-server
     :access-key (or (System/getenv "DATOMIC_ACCESS_KEY") "secretaccess")
     :secret (or (System/getenv "DATOMIC_SECRET") "secretsecret")
     :endpoint (str host ":" port)
     :validate-hostnames false}))

(def db-name (or (System/getenv "DATOMIC_DISCUSSION_DB_NAME") "dev-db"))