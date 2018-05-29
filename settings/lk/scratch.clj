;; Eval this when starting
(do
  (ns scratch
    (:require [clojure.test :as test]
              [clojure.repl :as repl]
              [clojure.tools.namespace.find :as ns.find]
              [clojure.tools.namespace.repl :as ns.repl]
              [clojure.java.io :as io])
    (:import (java.io File)))
  (defn list-ns-in-pj []
    (ns.find/find-namespaces-in-dir (File. "./src/")))

  (defn t [p]
    (test/run-all-tests (re-pattern p))))

;; Have fun!
