(ns scratch
  (:require [clojure.test :as test]
            [clojure.repl :as repl]
            [kaocha.repl]
            clojure.pprint
            [clojure.tools.namespace.find :as ns.find]
            [clojure.tools.namespace.repl :as ns.repl]
            [clojure.java.io :as io])
  (:import (java.io File)))

(def multithread (atom false))
(defn init! []
  (ns scratch)
  (ns.repl/disable-reload! *ns*)
  (println (str ">>> in ns " *ns*))
  (println "(scratch/t) - run all tests
(scratch/t \".*some-ns.*\") - run tests for matching namespaces
(scratch/time+ \"tag\"  (some expr)) - like time, but better
(scratch/refresh) - refresh all namespaces
(scrtach/refresh-all) - refresh all project + dep namespaces
(scratch/pp) alias for clojure.pprint/pprint
(scratch/list-ns) - find all namespaces in SRC"))

(def refresh ns.repl/refresh)
(def refresh-all ns.repl/refresh-all)
(def pp clojure.pprint/pprint)

(defn list-ns
  "Return list of symbols of namespaces found in src dir"
  ([root]
   (ns.find/find-namespaces-in-dir (File. root)))
   ([]
    (list-ns "./src/")))

(defn find-ns [re]
  (filter #(re-find re (str %)) (list-ns)))

(defn find-test-ns [re]
  (filter #(re-find re (str %)) (list-ns "./test/"))))

(defn t
  ([]
   (kaocha.repl/run :unit))
  ([& an-ns]
   (apply kaocha.repl/run an-ns)))

(defmacro time+
  "Like time but:
  - accepts a `tag` argument
  - prints out start + end time
  - rounds up ms - less precise"
  [tag & body]
  (printf "start |%s| %s\n"  tag (java.time.LocalDateTime/now))
  `(let [start-time# ^Long (System/currentTimeMillis)
         return# (do
                   ~@body)
         time# ^Long (- (System/currentTimeMillis) start-time#)]
     (printf "end |%s| %s - %sms\n" ~tag   (str (java.time.LocalDateTime/now)) time#)
     return#))

(init!)
