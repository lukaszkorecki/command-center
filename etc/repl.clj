(ns ^{:clojure.tools.namespace.repl/load false} R
  (:refer-clojure :exclude [find-ns])
  (:require
    [clojure.pprint]
    [clojure.tools.namespace.find :as ns.find]
    [clojure.tools.namespace.repl :as ns.repl]
    [kaocha.repl]
    [clojure.string :as str])
  (:import
    (java.io
      File)))


(ns.repl/disable-reload! *ns*)

(defn pp
  [thing]
  (clojure.pprint/pprint thing)
  thing)


(defn help [& _n]
  (println (str ">>> in ns " 'R))
  (mapv (fn [[_k v]]
          (println v)) (ns-publics 'R))
  ::ok)


(defn init! []
  (ns.repl/disable-reload! *ns*)
  (ns.repl/set-refresh-dirs "src" "test")
  (help))


(defn list-ns
  "Return list of symbols of namespaces found in src dir"
  ([root]
   (ns.find/find-namespaces-in-dir (File. root)))
  ([]
   (list-ns "./src/")))


(defn find-ns
  "Find namespace vars by a regex"
  [re]
  (vec (filter #(re-find re (str %)) (list-ns))))


(defn find-test-ns
  "Find test namespace vars by a regex"
  [pattern]
  (let [re (cond
             (string? pattern) (re-pattern pattern)
             (= java.util.regex.Pattern (class pattern)) pattern
             :else (throw (ex-info "this is not a patternable thing" {:pattern pattern})))]
  (vec (filter #(re-find re (str %)) (list-ns "./test/")))))


(def system-status (atom {}))


(defn safe-to-refresh? []
  (or (empty? @system-status)
      (= #{false} (-> @system-status vals set))))


(defn  refresh
  "Refresh changed namespaces"
  []
  (if (safe-to-refresh?)
    (ns.repl/refresh)
    ::system-running!))


(defn refresh-all
  "Refresh everything"
  []
  (if (safe-to-refresh?)
    (ns.repl/refresh-all)
    ::system-running!))


(defn start-system!
  "Given a namespace, usually some-service, do the following:
  - find some-service.user namespace (by convention)
  - refresh
  - require the user ns e.g. some-service.user
  - start  system, invoking somer-service.user/start
  Warning: best if the system is not running, or things will go south

  Example: (R/start-system! 'foo.user)"
  ([]
   ;; automagically guess the <app>.user namespace
   (let [an-ns (-> *ns*
                   str
                   (str/replace #"\..+" ".user")
                   symbol)]
     (require an-ns)
     (start-system! an-ns)))
  ([an-ns]
     (printf "!! Starting %s\n" an-ns)
     (if (get @system-status an-ns)
       (println "!! System possibly running" an-ns)
       (do
         (println "!! Refreshing and reloading " an-ns)
         (remove-ns an-ns)
         (refresh)
         (require [an-ns] :reload)
         (if-let [f (ns-resolve an-ns 'start)]
           (do
             (f)
             (swap! system-status (fn [s] (assoc s an-ns true)))))))))


(defn stop-system!
  "Given a namespace, usually some-service.user, stop the system. If not passed, stops currently running system"
  ([]
   (stop-system! (first (keys @system-status))))
  ([an-ns]
   (let [f (ns-resolve an-ns 'stop)]
     (f)
     (swap! system-status (fn [s] (assoc s an-ns false))))))


(defn sys
  "Pull out the system for passing around"
  []
  (var-get (ns-resolve (first (keys @system-status)) 'SYS)))


(defn c
  "Pul out a compont from a running system"
  [component-name]
  (let [sys (sys)]
    (get sys component-name)))



(defn t
  "Run tests via kaocha - either all or a list of vars"
  ([]
   (kaocha.repl/run :unit {:config "/home/ubuntu/.emacs.d/etc/kaocha.edn"}))
  ([ns-list]
   (apply kaocha.repl/run (conj ns-list {:config "/home/ubuntu/.emacs.d/etc/kaocha.edn"}))))


(defn t!
  "Run tests via kaocha, but refresh first - runs all tests or a list of vars"
  ([]
   (refresh)
   (kaocha.repl/run :unit {:config "/home/ubuntu/.emacs.d/etc/kaocha.edn"}))
  ([ns-list]
   (refresh)
   (apply kaocha.repl/run (conj ns-list {:config "/home/ubuntu/.emacs.d/etc/kaocha.edn"}))))


(defn clear-aliases
  "Reset aliases for given ns or current if no args given"
  ([]
  (clear-aliases *ns*))
  (
  [an-ns]
  (mapv #(ns-unalias an-ns %) (keys (ns-aliases an-ns)))))

(init!)
