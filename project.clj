(defproject de.npexception/spacetraders-bot "0.1.0-SNAPSHOT"
  :description "A bot to play https://spacetraders.io/"
  :url "https://github.com/NPException/spacetraders-bot"
  :license "MIT License"
  :plugins [[lein-pprint "1.3.2"]]
  :dependencies [[org.clojure/clojure "1.10.2"]
                 [de.npexception/spacetraders-clj "1.2.1"]]
  :global-vars {*warn-on-reflection* true}
  :repl-options {:init-ns user}
  :source-paths ["src"]
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
