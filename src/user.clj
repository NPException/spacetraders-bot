(ns user
  (:require [de.npexception.spacetraders-clj.core-with-token :as api :refer :all]))


(def PRIME "OE-PM")
(def TRITUS "OE-PM-TR")


(defn fuel-in-tank [ship]
  (-> ship
      :cargo
      (->> (filter #(-> % :good (= "FUEL"))))
      first
      :quantity
      (or 0)))


(defn buy-fuel! [ship amount]
  (:ship (place-purchase-order! ship :fuel amount)))


(defn adjust-fuel-in-ship
  [ship fuel-amount]
  (if (= fuel-amount 0)
    (update ship :cargo #(filterv (fn [item] (-> item :good (not= "FUEL"))) %))
    (update ship :cargo (fn [cargo]
                          (if (some #(-> % :good (= "FUEL")) cargo)
                            (mapv (fn [{good :good :as item}]
                                    (if (= good "FUEL")
                                      (assoc item :quantity fuel-amount
                                                  :totalVolume fuel-amount)
                                      item))
                                  cargo)
                            (conj cargo {:good "FUEL", :quantity fuel-amount, :totalVolume fuel-amount}))))))


(def fuel-regex #"You require (\d+) more FUEL")

(defonce fuel-usage (atom {}))

(defn calc-fuel-requirement!
  "Jettisons all fuel in the ship, to calculate the exact amount needed for the given route"
  [{type :type from :location :as ship} to]
  (println "Calculating fuel requirement for ship" type "from" from "to" to)
  (let [in-tank (fuel-in-tank ship)]
    (when (> in-tank 0)
      (jettison-cargo! ship :fuel (fuel-in-tank ship))))
  (let [required-fuel (-> (create-flight-plan! ship to) :error :message
                          (->> (re-find fuel-regex))
                          second
                          Integer/parseInt)]
    (swap! fuel-usage assoc [type from to] required-fuel))
  (update ship :cargo #(vec (remove (comp #{"FUEL"} :good) %))))


(defn refuel! [{type :type from :location :as ship} to]
  (if-let [required (some-> (@fuel-usage [type from to])
                            (- (fuel-in-tank ship)))]
    (if (> required 0)
      (buy-fuel! ship required)
      ship)
    (recur (calc-fuel-requirement! ship to) to)))


(defn at-location? [ship location]
  (= location (:location ship)))


(defn fly-to! [ship location]
  (let [ship (refuel! ship location)
        plan (:flightPlan (create-flight-plan! ship location))
        flight-ms (-> plan :timeRemainingInSeconds (+ 1) (* 1000))]
    (Thread/sleep flight-ms)
    (-> (adjust-fuel-in-ship ship (:fuelRemaining plan))
        (assoc :location (:destination plan)))))


(defn start-metal-trade!
  [ship]
  (let [running (volatile! true)
        ship (volatile! (:ship (api/my-ship-info ship)))]
    (future
      (println "Starting metal trade route" (:id @ship))
      (while @running
        (when-not (at-location? @ship TRITUS)
          (vswap! ship fly-to! TRITUS))
        ;; TODO: fill cargo bay with metals
        (vswap! ship fly-to! PRIME)
        ;; TODO sell metals
        ))
    #(vreset! running false)))
