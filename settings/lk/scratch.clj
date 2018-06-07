(ns scratch
  (:require [clojure.test :as test]
            [clojure.repl :as repl]
            [clojure.tools.namespace.find :as ns.find]
            [clojure.tools.namespace.repl :as ns.repl]
            [clojure.java.io :as io])
  (:import (java.io File)))

(defn list-ns-in-pj []
  (ns.find/find-namespaces-in-dir (File. "./src/")))

(defn list-test-ns-in-pj []
  (ns.find/find-namespaces-in-dir (File. "./test/")))

(defn require-all []
  (apply require (conj (vec (list-ns-in-pj)) :reload))
  (apply require (conj (vec (list-test-ns-in-pj)) :reload)))

(defn t
  ([]
   (require-all)
   (apply test/run-tests (list-test-ns-in-pj)))
  ([p]
   (let [test-nss (->> (list-test-ns-in-pj)
                       (filter #(re-find (re-pattern p) (str %))))]
     (apply require (conj (vec test-nss) :reload))
     (test/run-all-tests (re-pattern p)))))
