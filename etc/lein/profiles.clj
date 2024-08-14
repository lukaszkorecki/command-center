{:user {:dependencies [[org.clojars.lukaszkorecki/rumble "1.0.0.24"]]

        :aliases {;; do not override `lein test`
                  "kaocha" ["run" "-m" "kaocha.runner"]

                  "outdated-check" ["with-profile" "antq"
                                    "run" "-m" "antq.core"]

                  "outdated" ["with-profile" "antq"
                              "run" "-m" "antq.core" "--upgrade" "--force"]

                  "cloverage" ["with-profile" "+coverage"
                               "cloverage"
                               "-e" ".*dev.*"
                               "-e" ".*repl.*"
                               "-e" ".*benchmark.*"]}}

 :repl {:dependencies [[org.clojars.lukaszkorecki/rumble "1.0.0.24"]]
        :repl-options {:color false}}

 :antq {:global-vars {*warn-on-reflection* false}
        :dependencies [[com.github.liquidz/antq "2.7.1133"]
                       ;; silencelogs from antq
                       ^:displace [org.slf4j/slf4j-api "RELEASE"]
                       [org.slf4j/slf4j-nop "2.0.9"]]}

 :coverage {:plugins [[lein-cloverage "1.2.2"]]}}
