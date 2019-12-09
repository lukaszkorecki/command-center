{:repl {:dependencies [^:displace [org.clojure/clojure "1.10.1"]
                       [com.cemerick/pomegranate "1.1.0"]
                       [lambdaisland/kaocha "0.0-554"]
                       [nrepl "0.6.0"]
                       [org.clojure/tools.namespace "0.3.2-SNAPSHOT"]]
        :repl-options {:color false}}
 :user {:dependencies [^:displace [org.clojure/clojure "1.10.1"]
                       [cljfmt "0.6.6"]]
        :plugins [[lein-cljfmt "0.6.6"]
                  [lein-ancient "0.6.15"]]}}
