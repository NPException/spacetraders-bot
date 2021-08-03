(ns de.npexception.spacetraders-bot.trade
  (:require [de.npexception.spacetraders-clj.core-with-token :as api]
            [clojure.string :as str]))

(defonce
  ^{:doc "Returns a map of all goods available in the game."
    :arglists '([])}
  all-goods
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
(defn purchase-by-volume!
  [ship good target-volume]
  (let [good (str/upper-case (name good))
        vol-per-unit (:volumePerUnit ((all-goods) good))
        num-to-buy (quot target-volume vol-per-unit)]
    (reduce
      (fn [ship quantity]
        (:ship (api/place-purchase-order! ship good quantity)))
      ship
      (determine-order-batches ship num-to-buy))))


(defn purchase-all!
  [ship-data good]
  (purchase-by-volume! ship-data good (:spaceAvailable ship-data)))


(defn sell-all!
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


(def ^:private default-max-market-age-millis 30000)

(defonce ^:private all-markets-atom (atom {}))


(defn ^:private update-market-data
  "When the market for the given location is older than the
  given timestamp or does not exist yet, new data is fetched via the API."
  [all-markets location-symbol last-acceptable-timestamp]
  (let [market (get all-markets location-symbol)]
    (if (some-> (:market/timestamp market) (> last-acceptable-timestamp))
      all-markets
      (let [new-market (some-> (api/location-marketplace location-symbol)
                                  :marketplace
                                  (->> (map (juxt :symbol identity))
                                       (into {})))]
        (assoc all-markets
          location-symbol
          (some-> new-market (assoc :market/timestamp (System/currentTimeMillis))))))))


(defn market
  "If known, returns the market for the given location.
  The data will be at most max-market-age milliseconds old."
  ([location-symbol]
   (market location-symbol default-max-market-age-millis))
  ([location-symbol max-market-age]
   (let [market (get @all-markets-atom location-symbol)
         last-acceptable-timestamp (- (System/currentTimeMillis) max-market-age)]
     (if (some-> (:market/timestamp market) (> last-acceptable-timestamp))
       market
       (get (swap! all-markets-atom update-market-data location-symbol last-acceptable-timestamp)
            location-symbol)))))


(defn trading-data
  "If known, returns the trading data for the good at location.
  The data will be at most max-market-age milliseconds old."
  ([location-symbol good]
   (trading-data location-symbol good default-max-market-age-millis))
  ([location-symbol good max-market-age]
   (get (market location-symbol max-market-age)
        (str/upper-case (name good)))))
