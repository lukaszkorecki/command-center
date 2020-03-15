{:repl {:dependencies [^:displace [org.clojure/clojure "1.10.1"]
                       [lambdaisland/kaocha "0.0-601"]
                       [nrepl "0.6.0"]
                       [org.clojure/tools.namespace "0.3.1"]]
        :repl-options {:color false}}
 :user {:dependencies [^:displace [org.clojure/clojure "1.10.1"]
                       [cljfmt "0.6.6"]]
        :plugins [[lein-cljfmt "0.6.6"]
                  [lein-ancient "0.6.15"]]}

 :kaocha {:dependencies [[lambdaisland/kaocha "0.0-601"]]}
 }
