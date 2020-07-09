(ns dialog.engine.cli
  (:require [dialog.discussion.database :as database]
            [dialog.engine.core :as engine]
            [clojure.string :as string]))

(defn- list-discussion [discussions]
  (string/join
    "\n"
    (map-indexed
      (fn [idx [_id title]] (format "[%s] %s\n" idx title))
      discussions)))

(defn start []
  (let [discussions (database/all-discussion-titles-and-ids)]
    (println
      "Welcome 🥳! Choose a discussion:\n"
      (list-discussion discussions))
    (let [index (Integer/parseInt (read-line))
          [id title] (nth discussions index)]
      {:discussion/id id
       :discussion/title title})))

(defn choose-discussion [args]
  (engine/start-discussion args))



(comment
  (let [args (start)]
    (choose-discussion args))
  :end)