(ns scratch
  (:require [clojure.test :as test]
            [clojure.repl :as repl]
            [clojure.tools.namespace.find :as ns.find]
            [clojure.tools.namespace.repl :as ns.repl]
            [clojure.java.io :as io])
  (:import (java.io File)))

(defn list-ns-in-pj
  "Return list of symbols of namespaces found in src dir"
  []
  (ns.find/find-namespaces-in-dir (File. "./src/")))

(defn list-test-ns-in-pj
  "Return list of symbols of namespaces found in test dir"
  []
  (ns.find/find-namespaces-in-dir (File. "./test/")))

(defn require+reload
  "Requires and reloads all project namespaces"
  []
  (apply require (conj (vec (list-ns-in-pj)) :reload)))

(defn require+reload-tests
  "Requires and reloads all tests"
  []
  (apply require (conj (vec (list-test-ns-in-pj)) :reload)))

(defn require-all
  "Require+Reload all project and test namespaces"
  []
  (require+reload)
  (require+reload-tests))

(defn t
  "Reload and run tests. Without arguments run all tests.
  If argument is passed (String) is interpreted as a regex to
  find a namespace"
  ([]
   (require-all)
   (apply test/run-tests (list-test-ns-in-pj)))
  ([pattern]
   (let [regex (re-pattern pattern)
         test-nss (->> (list-test-ns-in-pj)
                       (filter #(re-find regex (str %))))]
     (require+reload)
     (apply require (conj (vec test-nss) :reload))
     (test/run-all-tests regex))))
