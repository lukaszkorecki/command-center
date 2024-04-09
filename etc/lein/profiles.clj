{:repl {:dependencies [[org.clojars.lukaszkorecki/rumble "1.0.0.24"]]
        :repl-options {:color false}}
 :user {:dependencies [^:displace [org.clojure/clojure "1.11.1"]
                       [org.clojars.lukaszkorecki/rumble "1.0.0.24"]]
        :aliases {"kaocha" ["run" "-m" "kaocha.runner"]
                  "outdated-check" ["with-profile" "+antq,+dev" "run" "-m" "antq.core"]
                  "outdated" ["with-profile" "+antq" "+dev" "run" "-m" "antq.core" "--upgrade" "--force"]}}

 :antq {:dependencies [[com.github.liquidz/antq "RELEASE"]
                       [org.yaml/snakeyaml "2.2"]
                       [org.slf4j/slf4j-nop "RELEASE"]]}}
