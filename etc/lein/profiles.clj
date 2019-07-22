{:repl {:dependencies [^:displace [org.clojure/clojure "1.10.0"]
                       [com.cemerick/pomegranate "0.4.0"]
                       [lambdaisland/kaocha "0.0-389"]
                       [org.clojure/tools.namespace "0.3.0"]]
        :repl-options {:color false}}
 :user {:dependencies [^:displace [org.clojure/clojure "1.10.0"]

                       [cljfmt "0.6.4"]
                       [slamhound "1.5.5"]]
        :aliases {"slamhound" ["run" "-m" "slam.hound"]}
        :plugins [[lein-cljfmt "0.6.4"]
                  [lein-cloverage "1.1.1"]
                  [lein-ancient "0.6.15"]]}}
