{:repl {:dependencies [[org.clojars.lukaszkorecki/rumble "0.1.0-SNAPSHOT-6"]
                       ^:displace [nrepl "0.9.0"]]
        :repl-options {:color false}}
 :user {:dependencies [^:displace [org.clojure/clojure "1.11.1"]
                       [lambdaisland/kaocha "1.66.1034"] ; for the alias
                       ]
        :aliases {"kaocha" ["run" "-m" "kaocha.runner"]}
        :plugins [[lein-ancient "1.0.0-RC3"]
                  [lein-nvd "1.7.0"]
                  [lein-licenses "0.2.2"]]}}
