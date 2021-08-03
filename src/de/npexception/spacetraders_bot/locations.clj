(ns de.npexception.spacetraders-bot.locations
  (:require [de.npexception.spacetraders-clj.core-with-token :as api]))

(defonce
  ^{:doc "Returns a map of all locations in the given system."
    :arglists '([system])}
  locations-in-system
  (memoize
    (fn [system]
      (->> (api/system-locations system)
           :locations
           (map (juxt :symbol identity))
           (into {})))))


(defonce
  ^{:doc "Returns the location data map for a specific location symbol."
    :arglists '([location-symbol])}
  location-data
  (fn [location-symbol]
    (-> (subs location-symbol 0 2)
        locations-in-system
        (get location-symbol))))
