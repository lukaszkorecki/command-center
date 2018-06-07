{:user {:dependencies [[org.clojure/tools.namespace "0.3.0-alpha4"]]
        :repl-options {:color false}
;;        :jvm-opts ["-Dclojure.server.repl={:port 5555 :accept clojure.core.server/repl}"]
        :plugins [[lein-cljfmt "0.4.1" :exclusions [org.clojure/clojure]]
                  [lein-cloverage "1.0.6" :exclusions [org.clojure/clojure]]
                  [lein-ancient "0.6.15", :exclusions [org.clojure/clojure]]]}}
