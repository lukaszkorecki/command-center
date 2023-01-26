{:repl {:dependencies [^:displace [org.clojure/clojure "1.11.1"]
                       [nrepl "0.8.3"]]
        :repl-options {:color false}}
 :user {:dependencies [^:displace [org.clojure/clojure "1.11.1"]
                       [org.clojars.lukaszkorecki/rumble "0.1.0-SNAPSHOT-3"]]
        :aliases {"kaocha" ["run" "-m" "kaocha.runner"]}
        :injections [ (require '[rumble.repl :as R])]
        :plugins [[lein-ancient "1.0.0-RC3"]
                  [lein-nvd "1.7.0"]
                  [lein-licenses "0.2.2"]]}}
