{:repl {:dependencies [^:displace [org.clojure/clojure "1.9.0"]
                       [org.clojure/tools.namespace "0.3.0-alpha4"]
                       [eftest "0.5.2"]]
        :repl-options {:color false}}
 :user {:plugins [[lein-cljfmt "0.4.1" :exclusions [org.clojure/clojure]]
                  [lein-cloverage "1.0.6" :exclusions [org.clojure/clojure]]
                  [lein-kibit "0.1.3" :exclusions [org.clojure/clojure]]
                  [lein-eftest "0.5.2" :exclusions [org.clojure/clojure]]
                  [lein-ancient "0.6.15", :exclusions [org.clojure/clojure]]]}}
