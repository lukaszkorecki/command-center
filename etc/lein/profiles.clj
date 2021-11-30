{:repl {:dependencies [^:displace [org.clojure/clojure "1.10.3"]
                       [nrepl "0.8.3"]
                       [org.clojure/tools.namespace "1.1.0"]]
        :repl-options {:color false}}
 :user {:dependencies [^:displace [org.clojure/clojure "1.10.3"]
                       [lambdaisland/kaocha "1.60.945"]]
        :aliases {"kaocha" ["run" "-m" "kaocha.runner"]}
        :plugins [[lein-ancient "1.0.0-RC3"]
                  [lein-nvd "1.7.0"]
                  [lein-licenses "0.2.2"]]}}
