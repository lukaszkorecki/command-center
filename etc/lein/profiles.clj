{:repl {:dependencies [^:displace [org.clojure/clojure "1.10.1"]
                       [com.cemerick/pomegranate "1.1.0"]
                       [lambdaisland/kaocha "0.0-554"]
                       [org.clojure/tools.namespace "0.3.1"]]
        :repl-options {:color false}}
 :user {:dependencies [^:displace [org.clojure/clojure "1.10.1"]
                       [cljfmt "0.6.4"]]
        :plugins [[lein-cljfmt "0.6.4"]
                  [lein-cloverage "1.1.1"]
                  [lein-ancient "0.6.15"]]}}
