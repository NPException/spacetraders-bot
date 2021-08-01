(ns user
  (:require [de.npcomplete.spacetraders-clj.core-with-token :as api]))


(defn start-metal-trade!
  [ship]
  (let [running (volatile! true)]
    (future
      (println "Starting metal trade route" (or (:id ship) ship))
      (while @running
        ;; TODO: just run metal from OE-PM-TR to OE-PM continuously
        ))
    #(vreset! running false)))
