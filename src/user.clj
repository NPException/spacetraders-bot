(ns user
  (:require [de.npexception.spacetraders-clj.core-with-token :as api]
            [de.npexception.spacetraders-bot.travel :as travel]
            [de.npexception.spacetraders-bot.trade :as trade]))


(defn fuel-cheaper-than
  "Returns true, if fuel can be purchased cheaper at loc-a than loc-b"
  [loc-a loc-b]
  (boolean
    (some-> (:purchasePricePerUnit (trade/trading-data loc-a :fuel 300000))
            (< (or (:purchasePricePerUnit (trade/trading-data loc-b :fuel 300000))
                   0)))))


(defn start-trade-route!
  "Trader route between to locations. (both must have fuel supply!)"
  [ship location-1 location-2 good-1 good-2]
  (let [running (volatile! true)
        ship (volatile! (:ship (api/my-ship-info ship)))
        ship-id (:id @ship)]
    {:ship ship
     :stop #(vreset! running false)
     :future (future
               (Thread/sleep 1000)
               (println "Starting trade route for" ship-id "between" location-1 "and" location-2 "with" good-1 "and" good-2)
               (while @running
                 ;; fly to first location
                 (println "Ship" ship-id "flying to" location-1)
                 (vswap! ship travel/fly-to! location-1)
                 ;; sell second good when necessary
                 (when good-2
                   (println "Ship" ship-id "selling" good-2)
                   (vswap! ship trade/sell-all! good-2))
                 ;; try purchase round-trip fuel if it's cheaper here
                 (when (and (travel/known-roundtrip-fuel? @ship location-1 location-2)
                            (fuel-cheaper-than location-1 location-2))
                   (println "Ship" ship-id "buying roundtrip fuel at" location-1)
                   (vswap! ship travel/refuel-for-roundtrip! location-1 location-2))
                 ;; purchase fuel for trip to second location as necessary
                 (vswap! ship travel/refuel-for-trip! location-2)
                 ;; buy first good
                 (println "Ship" ship-id "buying" good-1)
                 (vswap! ship trade/purchase-all! good-1)
                 ;; fly to second location
                 (println "Ship" ship-id "flying to" location-2)
                 (vswap! ship travel/fly-to! location-2)
                 ;; sell first good
                 (println "Ship" ship-id "selling" good-1)
                 (vswap! ship trade/sell-all! good-1)
                 ;; try purchase round-trip fuel if it's cheaper here (no need for safety checks, since we know both locations now
                 (when (fuel-cheaper-than location-2 location-1)
                   (println "Ship" ship-id "buying roundtrip fuel at" location-2)
                   (vswap! ship travel/refuel-for-roundtrip! location-1 location-2))
                 ;; purchase fuel for trip back to first location as necessary
                 (vswap! ship travel/refuel-for-trip! location-1)
                 ;; buy second good when necessary
                 (when good-2
                   (println "Ship" ship-id "buying" good-2)
                   (vswap! ship trade/purchase-all! good-2))))}))


(defn start-tritus-metal-route
  [ship]
  (start-trade-route! ship "OE-PM-TR" "OE-PM" "METALS" nil))
