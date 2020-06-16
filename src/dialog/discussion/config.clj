(ns dialog.discussion.config)

;; Dev config. Need a proper way to handle switch when in production.
;; ##################################################################
(def datomic
  {:server-type :peer-server
   :access-key "secretaccess"
   :secret "secretsecret"
   :endpoint "localhost:8998"
   :validate-hostnames false})

(def db-name "dev-db")