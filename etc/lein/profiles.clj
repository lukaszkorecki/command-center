{:repl {:dependencies [[org.clojars.lukaszkorecki/rumble "0.1.0-SNAPSHOT-2"]
                       ^:displace [nrepl "0.9.0"]]
        :injections [(require '[rumble.repl :as R])]
        :repl-options {:color false}}
 :dev {:injections [(require '[rumble.repl :as R])]}
 :user {:dependencies [^:displace [org.clojure/clojure "1.10.3"]
                       [lambdaisland/kaocha "1.63.998"] ;; for the alias
                       ]
        :aliases {"kaocha" ["run" "-m" "kaocha.runner"]}
        :plugins [[lein-ancient "1.0.0-RC3"]
                  [lein-nvd "1.7.0"]
                  [lein-licenses "0.2.2"]]}}
