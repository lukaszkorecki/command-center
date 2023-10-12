(ns ^{:clojure.tools.namespace.repl/load false} r
  (:refer-clojure :exclude [find-ns])
  (:require
   [clojure.java.browse]
   [clojure.java.io :as io]
   [clojure.pprint]
   [clojure.repl]
   [clojure.string :as str]
   [clojure.tools.namespace.find :as ns.find]
   [clojure.tools.namespace.repl :as ns.repl]
   [kaocha.repl]
   [portal.api]
   [portal.colors])
  (:import
   (java.io
    File)))

(ns.repl/disable-reload! *ns*)

(def ^{:doc "Pretty print given thing"} ppn clojure.pprint/pprint)

;; debugging and stuff
(defn pp
  "Like ppn, but returns passed in data. Useful for debugging threaded calls"
  [thing]
  (ppn thing)
  thing)

;; finding things in a Clj project
(defn list-ns
  "Return list of symbols of namespaces found in src dir. Default: ./src"
  ([root]
   (ns.find/find-namespaces-in-dir (File. root)))
  ([]
   (list-ns "./src/")))

(defn find-ns
  "Find namespace vars by a regex"
  [re]
  (let [nss (vec (filter #(re-find re (str %)) (list-ns)))]
    (printf ";; found %s ns\n" (count nss))
    (when (<= (count nss) 10)
      (for [n nss]
        (printf ";; %s\n" n)))
    nss))

(defn tests
  "Find test namespace vars by a regex"
  [pattern]
  (let [re (cond
             (string? pattern) (re-pattern pattern)
             (= java.util.regex.Pattern (class pattern)) pattern
             :else (throw (ex-info "this is not a patternable thing" {:pattern pattern})))
        nss (vec (filter #(re-find re (str %)) (list-ns "./test/")))]
    (printf ";; found %s nss\n" (count nss))
    (when (<= (count nss) 10)
      (for [n nss]
        (printf ";; %s\n" n)))
    nss))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(def find-test-ns tests)

(defn describe-ns
  "Describes given namespace by listing PUBLIC symbols, optionally filters down via :s <search>
  and can optionally add the doc string with :doc true"
  [an-ns & {:keys [s docs?]}]
  (->> an-ns
       (ns-publics)
       (filter (fn [[sym _thing]]
                 (if s
                   (re-find (if (string? s)
                              (re-pattern s)
                              s)
                            (str sym))
                   true)))
       (sort-by #(str (first %)))
       (mapv (fn [[sym thing]]
               (format "> %s > %s > %s%s"
                       sym
                       (str thing)
                       (get (meta thing) :arglists)
                       (if docs?
                         (str "\n" (get (meta thing) :doc) "\n")
                         ""))))
       (clojure.string/join "\n")
       (println)))

(def ^:private system-status (atom {}))

(defn safe-to-refresh?
  "Check if refresh is safe, by verifying that application system is not running"
  []
  (or (empty? @system-status)
      (= #{false} (-> @system-status vals set))))

(defn refresh
  "Refresh changed namespaces, only if its safe"
  []
  (if (safe-to-refresh?)
    (ns.repl/refresh)
    ::system-running!))

(defn refresh-all
  "Refresh everything, only if its safe"
  []
  (if (safe-to-refresh?)
    (ns.repl/refresh-all)
    ::system-running!))

(defn system-ns []
  (-> *ns*
      str
      (str/replace #"\..+" ".user")
      symbol))

(defn start-system!
  "Given a namespace, usually some-service, do the following:
  - find some-service.user namespace (by convention)
  - refresh
  - require the user ns e.g. some-service.user
  - start  system, invoking somer-service.user/start
  Warning: best if the system is not running, or things will go south

  Example: (r/start-system! 'foo.user)"
  ([]
   ;; automagically guess the <app>.user namespace
   (let [an-ns (system-ns)]
     (require an-ns)
     (start-system! an-ns)))
  ([an-ns]
   (printf ";; Starting %s\n" an-ns)
   (when (= "r" (str an-ns))
     (throw (ex-info "nope" {:ns (str an-ns)})))
   (if (get @system-status an-ns)
     (println ";; System possibly running" an-ns)
     (do
       (println ";; Refreshing and reloading " an-ns)
       (remove-ns an-ns)
       (refresh)
       (require [an-ns] :reload)
       (when-let [f (ns-resolve an-ns 'start)]
         (f)
         (swap! system-status (fn [s] (assoc s an-ns true))))))))

(defn stop-system!
  "Given a namespace, usually some-service.user, stop the system. If not passed, stops currently running system"
  ([]
   (stop-system! (first (keys @system-status))))
  ([an-ns]
   (let [f (ns-resolve an-ns 'stop)]
     (f)
     (swap! system-status (fn [s] (assoc s an-ns false))))))

(defn restart-system!
  "Restarts the system with an optional reload. If the system is not running, it will start it"
  []
  (when (first (keys @system-status))
    (stop-system!))
  (start-system!))

(defn sys
  "Pull out the system for passing around"
  []
  (var-get (ns-resolve (first (keys @system-status)) 'SYS)))

(defn c
  "Pul out a compont from a running system, pass keyword for the component name"
  [component-name]
  (when-let [sys (sys)]
    (get sys component-name)))

;;; Test helpers

(def ^:private kaocha-conf {:config (System/getenv "KAOCHA_CONFIG")})

(defn t
  "Run tests via kaocha - either all or a list of vars. WILL NOT REFRESH"
  ([]
   (kaocha.repl/run :unit kaocha-conf))
  ([ns-list]
   (apply kaocha.repl/run (flatten [ns-list [kaocha-conf]]))))

(defn t!
  "Run tests via kaocha, but refresh first - runs all tests or a list (or one) of ns vars"
  ([]
   (println (refresh))
   (kaocha.repl/run :unit kaocha-conf))
  ([& ns-list]
   (println (refresh))
   (apply kaocha.repl/run (flatten [ns-list [kaocha-conf]]))))

(defn clear-aliases
  "Reset aliases for given ns or current if no args given"
  ([]
   (clear-aliases *ns*))
  ([an-ns]
   (mapv #(ns-unalias an-ns %) (keys (ns-aliases an-ns)))))

;; Tap helpers

(def ^:private tap-log (atom []))
(def ^:private tap-ref (atom nil))

(defn t>
  "Like tap> but returns input"
  [input]
  (tap> input)
  input)


(defn tap-log-init!
  "Initialize a tap> listener and store the ref to it"
  []
  (reset! tap-ref (add-tap (fn [input]
                             (swap! tap-log conj input)))))

(defn tap-log-get
  "Return tap logged data"
  []
  @tap-log)

(defn tap-log-clear!
  "Clear the log"
  []
  (reset! tap-log []))

(defn tap-log-stop!
  "Clear tap log and remove the listener"
  []
  (remove-tap @tap-ref)
  (tap-log-clear!)
  (reset! tap-ref nil))

(def portal-tap (atom nil))

(defn portal-start! []
  (let [url (if (.exists (io/file ".portal-url"))
              (do
                (println "Found .portal-url, using that")
                (slurp ".portal-url"))
              (do
                (println "No .portal-url found, starting portal")
              ;; Tweak Portal fonts because they're too big
                (let [tweaked-themes (assoc portal.colors/themes
                                            :portal.colors/nord-light-tweaked
                                            (merge (:portal.colors/nord-light portal.colors/themes)
                                                   {:font-size 8}))]
                  (with-redefs [portal.colors/themes tweaked-themes]
                    (portal.api/url (portal.api/open {:window-title "monroe portal"
                                                      :theme :portal.colors/nord-light-tweaked
                                                      :launcher false}))))))]
    (spit ".portal-url" url)
    (reset! portal-tap (add-tap #'portal.api/submit))
    (clojure.java.browse/browse-url url)))

(defn portal-clear! []
  (portal.api/clear))

(defn portal-stop! []
  (swap! portal-tap remove-tap)
  (io/delete-file ".portal-url")
  (portal.api/close))

;;

(defn help []
  (describe-ns *ns* :doc true))

(defn- init!
  "Initialize the helper namespace"
  []
  (ns.repl/disable-reload! *ns*)
  (ns.repl/set-refresh-dirs "src" "test")
  (describe-ns *ns* :docs? true))

(init!)
