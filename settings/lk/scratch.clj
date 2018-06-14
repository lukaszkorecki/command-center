(ns scratch
  (:require [clojure.test :as test]
            [clojure.repl :as repl]
            [clojure.tools.namespace.find :as ns.find]
            [clojure.tools.namespace.repl :as ns.repl]
            [clojure.java.io :as io])
  (:import (java.io File)))

(defn init! []
  (ns.repl/disable-reload!))

(def run-test test/run-all-tests)
(def refresh ns.repl/refresh)
(def refresh-all ns.repl/refresh-all)

(defn list-ns-in-pj
  "Return list of symbols of namespaces found in src dir"
  []
  (ns.find/find-namespaces-in-dir (File. "./src/")))

(defn list-test-ns-in-pj
  "Return list of symbols of namespaces found in test dir"
  []
  (ns.find/find-namespaces-in-dir (File. "./test/")))

(defn t
  "Reload and run tests. Without arguments run all tests.
  If argument is passed (String) is interpreted as a regex to
  find a namespace"
  ([]
   (refresh-all)
   (test/run-all-tests))
  ([pattern]
   (let [regex (re-pattern pattern)]
     (refresh)
     (test/run-all-tests regex))))

(println "yo")
(init!)
