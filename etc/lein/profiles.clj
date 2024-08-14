{:user {:dependencies [[org.clojars.lukaszkorecki/rumble "1.0.0.24"]]

        :aliases {"kaocha" ["run" "-m" "kaocha.runner"]
                  "outdated-check" ["with-profile" "antq" "run" "-m" "antq.core"]
                  "outdated" ["with-profile" "antq"
                              "run" "-m" "antq.core" "--upgrade" "--force"]

                  "cloverage" ["with-profile" "+coverage"
                               "cloverage" "-e" ".*dev.*" "-e" ".*repl.*" "-e" ".*benchmark.*"]}}

 :repl {:dependencies [[org.clojars.lukaszkorecki/rumble "1.0.0.24"]]
        :repl-options {:color false}}

 :antq {:dependencies [[com.github.liquidz/antq "RELEASE"]
                       ^:displace [org.slf4j/slf4j-api "RELEASE"]
                       [org.slf4j/slf4j-nop "RELEASE"]]}

 :coverage {:plugins [[lein-cloverage "1.2.2"]]}}
