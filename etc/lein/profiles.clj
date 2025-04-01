{:user {:dependencies [[org.clojars.lukaszkorecki/rumble "1.0.0.37"]]

        :repl-options {:color false}

        :aliases {;; do not override `lein test`
                  "kaocha" ["run" "-m" "kaocha.runner"]

                  "outdated-check" ["with-profile" "+antq,-dev,-repl,-cloverage,-system,-base,-user"
                                    "run" "-m" "antq.core"]

                  "outdated" ["with-profile" "antq"
                              "run" "-m" "antq.core" "--upgrade" "--force"]

                  "cloverage" ["with-profile" "+coverage"
                               "cloverage"
                               "-e" ".*dev.*"
                               "-e" ".*repl.*"
                               "-e" ".*benchmark.*"]}}

 :repl {:dependencies [[org.clojars.lukaszkorecki/rumble "1.0.0.37"]]
        :repl-options {:color false}}

 :antq {:global-vars {*warn-on-reflection* false}
        :dependencies [[com.github.liquidz/antq "RELEASE"]
                       ;; silencelogs from antq
                       ^:displace [org.slf4j/slf4j-api "RELEASE"]
                       [org.slf4j/slf4j-nop "2.0.9"]

                       ;; these are needed when running checks
                       ;; in repos with older dependencies
                       [org.clojure/tools.reader "1.4.2"]
                       [org.yaml/snakeyaml "2.2"]]}
 :coverage {:plugins [[lein-cloverage "1.2.2"]]}}
