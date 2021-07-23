{:repl {:dependencies [^:displace [org.clojure/clojure "1.10.1"]
                       [lambdaisland/kaocha "1.0.672"]
                       [nrepl "0.8.0"]
                       [org.clojure/tools.namespace "0.3.1"]]
        :repl-options {:color false}}
 :user {:dependencies [^:displace [org.clojure/clojure "1.10.3"]]
        :plugins [[lein-ancient "0.6.15"]
                  [lein-nvd "1.4.1"]
                  [lein-licenses "0.2.2"]]}
 :kaocha {:dependencies [[lambdaisland/kaocha "1.0.672"]]}}
