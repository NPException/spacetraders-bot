(ns user
  (:require [de.npexception.spacetraders-clj.core-with-token :as api]
            [clojure.string :as str]))


(defonce locations
  (memoize
    (fn [system]
      (->> (api/system-locations system)
           :locations
           (map (juxt :symbol identity))
           (into {})))))

(defn location-data
  [location]
  (-> (subs location 0 2)
      locations
      (get location)))

(defonce coordinates
  (memoize
    (fn [location]
      (-> (location-data location)
          (select-keys [:x :y])))))


(defn fuel-in-tank
  [ship]
  (-> ship
      :cargo
      (->> (filter #(-> % :good (= "FUEL"))))
      first
      :quantity
      (or 0)))


;; TODO: bear in mind ship :loadingSpeed
(defn buy-fuel!
  [ship amount]
  (:ship (api/place-purchase-order! ship :fuel amount)))


(defn adjust-fuel-in-ship
  [ship fuel-consumed fuel-remaining]
  (let [ship (update ship :spaceAvailable + fuel-consumed)]
    (if (= fuel-remaining 0)
      (update ship :cargo #(filterv (fn [item] (-> item :good (not= "FUEL"))) %))
      (update ship :cargo (fn [cargo]
                            (if (some #(-> % :good (= "FUEL")) cargo)
                              (mapv (fn [{good :good :as item}]
                                      (if (= good "FUEL")
                                        (assoc item :quantity fuel-remaining
                                                    :totalVolume fuel-remaining)
                                        item))
                                    cargo)
                              (conj cargo {:good "FUEL", :quantity fuel-remaining, :totalVolume fuel-remaining})))))))


(def fuel-regex #"You require (\d+) more FUEL")

(defonce fuel-usage (atom {}))

(defn calc-fuel-requirement!
  "Calculates how much fuel must be purchaes for the given trip.
  Returns a vector of the ship and the fuel requirement.
  If the fuel consumption for a given trip is not yet known, all fuel from the ship
  will be removed, in order to check via the API how much fuel a trip would need."
  [{type :type from :location :as ship} to]
  (if-let [trip-fuel (@fuel-usage [type from to])]
    [ship trip-fuel]
    (let [in-tank (fuel-in-tank ship)]
      (println "Calculating fuel requirement for ship type" type "from" from "to" to)
      ;; get rid of stored fuel when necessary
      (when (> in-tank 0)
        ;; TODO: sell instead of jetison
        (api/jettison-cargo! ship :fuel (fuel-in-tank ship)))
      ;; try to create flight plan to receive fuel requirements from error
      (let [required-fuel (-> (api/create-flight-plan! ship to) :error :message
                              (->> (re-find fuel-regex))
                              second
                              Integer/parseInt)]
        (swap! fuel-usage assoc [type from to] required-fuel)
        ;; make sure potential fuel data is removed from ship
        [(adjust-fuel-in-ship ship in-tank 0) required-fuel]))))


(defn refuel-for-trip!
  "Checks if there is enough fuel in the ship to reach the destination.
  Purchases fuel if not."
  [ship to]
  (let [[ship trip-fuel] (calc-fuel-requirement! ship to)
        required (- trip-fuel (fuel-in-tank ship))]
    (if (> required 0)
      (buy-fuel! ship required)
      ship)))


(defn at-location?
  [ship location]
  (= location (:location ship)))


(defn fly-to!
  [ship location]
  (if (at-location? ship location)
    ship
    (let [ship (refuel-for-trip! ship location)
          plan (:flightPlan (api/create-flight-plan! ship location))
          destination (:destination plan)
          flight-ms (-> plan :timeRemainingInSeconds (+ 1) (* 1000))]
      (Thread/sleep flight-ms)
      (-> (adjust-fuel-in-ship ship (:fuelConsumed plan) (:fuelRemaining plan))
          (assoc :location destination)
          (merge (coordinates destination))))))


(defonce all-goods
  (memoize
    (fn []
      (->> (api/available-goods)
           :goods
           (map (juxt :symbol identity))
           (into {})))))


(defn determine-order-batches
  [{:keys [loadingSpeed] :as _ship} total-amount]
  (if (<= total-amount loadingSpeed)
    [total-amount]
    (let [n (quot total-amount loadingSpeed)
          rem (mod total-amount loadingSpeed)]
      (concat (repeat n loadingSpeed)
              (when (> rem 0)
                [rem])))))


;; TODO: error handling
(defn buy!
  [ship good]
  (let [good (str/upper-case (name good))
        vol-per-unit (:volumePerUnit ((all-goods) good))
        num-to-buy (quot (:spaceAvailable ship) vol-per-unit)]
    (reduce
      (fn [ship quantity]
        (:ship (api/place-purchase-order! ship good quantity)))
      ship
      (determine-order-batches ship num-to-buy))))


(defn sell!
  [ship good]
  (let [good (str/upper-case (name good))
        num-to-sell (->> (:cargo ship)
                         (filter #(= (:good %) good))
                         first
                         :quantity)]
    (if-not num-to-sell
      ship
      (reduce
        (fn [ship quantity]
          (:ship (api/place-sell-order! ship good quantity)))
        ship
        (determine-order-batches ship num-to-sell)))))


(defn start-trade-route!
  [ship location-1 location-2 good-1 good-2]
  (let [running (volatile! true)
        ship (volatile! (:ship (api/my-ship-info ship)))
        ship-id (:id @ship)]
    {:ship ship
     :stop #(vreset! running false)
     :future (future
               (println "Starting trade route for" ship-id "between" location-1 "and" location-2 "with" good-1 "and" good-2)
               (while @running
                 ;; fly to first location
                 (println "Ship" ship-id "flying to" location-1)
                 (vswap! ship fly-to! location-1)
                 ;; sell second good when necessary
                 (when good-2
                   (println "Ship" ship-id "selling" good-2)
                   (vswap! ship sell! good-2))
                 ;; purchase fuel for trip to second location
                 (println "Ship" ship-id "fuelling up for flight to" location-2)
                 (vswap! ship refuel-for-trip! location-2)
                 ;; buy first good
                 (println "Ship" ship-id "buying" good-1)
                 (vswap! ship buy! good-1)
                 ;; fly to second location
                 (println "Ship" ship-id "flying to" location-2)
                 (vswap! ship fly-to! location-2)
                 ;; sell first good
                 (println "Ship" ship-id "selling" good-1)
                 (vswap! ship sell! good-1)
                 ;; purchase fuel for trip back to first location
                 (println "Ship" ship-id "fuelling up for flight to" location-1)
                 (vswap! ship refuel-for-trip! location-1)
                 ;; buy second good when necessary
                 (when good-2
                   (println "Ship" ship-id "buying" good-2)
                   (vswap! ship buy! good-2))))}))
