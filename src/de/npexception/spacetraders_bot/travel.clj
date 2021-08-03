(ns de.npexception.spacetraders-bot.travel
  (:require [de.npexception.spacetraders-clj.core-with-token :as api]
            [de.npexception.spacetraders-bot.locations :as locs]
            [de.npexception.spacetraders-bot.trade :as trade]))


(defn fuel-in-tank
  "Returns the amount of fuel in the given ship."
  [ship-data]
  (-> ship-data
      :cargo
      (->> (filter #(-> % :good (= "FUEL"))))
      first
      :quantity
      (or 0)))


(defn ^:private adjust-fuel-in-tank
  [ship-data fuel-consumed fuel-remaining]
  (let [ship-data (update ship-data :spaceAvailable + fuel-consumed)]
    (if (= fuel-remaining 0)
      (update ship-data :cargo #(filterv (fn [item] (-> item :good (not= "FUEL"))) %))
      (update ship-data :cargo (fn [cargo]
                                 (if (some #(-> % :good (= "FUEL")) cargo)
                                   (mapv (fn [{good :good :as item}]
                                           (if (= good "FUEL")
                                             (assoc item :quantity fuel-remaining
                                                         :totalVolume fuel-remaining)
                                             item))
                                         cargo)
                                   (conj cargo {:good "FUEL", :quantity fuel-remaining, :totalVolume fuel-remaining})))))))


(def ^:private fuel-regex #"You require (\d+) more FUEL")

(defonce ^:private fuel-usage (atom {}))


(defn fuel-requirement
  [ship-type from to]
  (@fuel-usage [ship-type from to]))


(defn ^:private save-fuel-requirement!
  [ship-type from to required-fuel]
  (swap! fuel-usage assoc [ship-type from to] required-fuel))


(defn ^:private calc-fuel-requirement!
  "Calculates how much fuel must be purchaes for the given trip.
  Returns a vector of the ship and the fuel requirement.
  If the fuel consumption for a given trip is not yet known, all fuel from the ship
  will be removed, in order to check via the API how much fuel a trip would need."
  [{type :type from :location :as ship-data} to]
  (if-let [trip-fuel (fuel-requirement type from to)]
    [ship-data trip-fuel]
    (let [in-tank (fuel-in-tank ship-data)]
      (println "Calculating fuel requirement for ship type" type "from" from "to" to)
      ;; get rid of stored fuel when necessary
      (when (> in-tank 0)
        ;; TODO: sell instead of jetison
        (api/jettison-cargo! ship-data :fuel (fuel-in-tank ship-data)))
      ;; try to create flight plan to receive fuel requirements from error
      (let [required-fuel (-> (api/create-flight-plan! ship-data to) :error :message
                              (->> (re-find fuel-regex))
                              second
                              Integer/parseInt)]
        (save-fuel-requirement! type from to required-fuel)
        ;; make sure potential fuel data is removed from ship
        [(adjust-fuel-in-tank ship-data in-tank 0) required-fuel]))))


(defn ^:private buy-fuel!
  [ship amount]
  (cond-> ship
    (> amount 0) (trade/purchase-by-volume! :fuel amount)))


(defn refuel-for-trip!
  "Checks if there is enough fuel in the ship to reach the destination.
  Purchases fuel if not."
  [ship-data to]
  (let [[ship-data trip-fuel] (calc-fuel-requirement! ship-data to)
        required (- trip-fuel (fuel-in-tank ship-data))]
    (buy-fuel! ship-data required)))


(defn known-roundtrip-fuel?
  "Checks if the roundtrip fuel requirements for
  the given ship type and locations is already known."
  ([{from :location :as ship-data} to]
   (known-roundtrip-fuel? ship-data from to))
  ([{type :type :as _ship-data} from to]
   (boolean (and (fuel-requirement type from to)
                 (fuel-requirement type to from)))))


(defn refuel-for-roundtrip!
  "Attempts to refuel the ship for the given roundtrip.
  Silently fails if roundtrip fuel requirements are not yet known."
  ([{from :location :as ship-data} to]
   (refuel-for-roundtrip! ship-data from to))
  ([{type :type :as ship-data} from to]
   (if-not (known-roundtrip-fuel? ship-data from to)
     ship-data
     (let [roundtrip-fuel (+ (fuel-requirement type from to)
                             (fuel-requirement type to from))
           required (- roundtrip-fuel (fuel-in-tank ship-data))]
       (buy-fuel! ship-data required)))))


(defn at-location?
  "Returns true if the ship is at the location specified by the given location"
  [ship-data location]
  (= (or (:symbol location) location)
     (:location ship-data)))


(defn fly-to!
  [ship-data location]
  (if (at-location? ship-data location)
    ship-data
    (let [ship-data (refuel-for-trip! ship-data location)
          plan (:flightPlan (api/create-flight-plan! ship-data location))
          destination (:destination plan)
          {dest-x :x dest-y :y} (locs/location-data destination)
          flight-ms (-> plan :timeRemainingInSeconds (+ 1) (* 1000))]
      (Thread/sleep flight-ms)
      (-> (adjust-fuel-in-tank ship-data (:fuelConsumed plan) (:fuelRemaining plan))
          (assoc :location destination
                 :x dest-x
                 :y dest-y)))))
