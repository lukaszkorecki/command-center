#!/usr/bin/env bb
(ns todo)
(require '[clojure.string :as str])
(import (java.net URLEncoder))

;; -*- mode: clojure -*-
;; Things url scheme is documented here:
;; https://culturedcode.com/things/support/articles/2803573/
;; all we're doing here is a script tha parses known markup
;; and encodes it into quick add URL
;; example:
;; things:///add?title=this%20is%20the%20task%20title&when=today&deadline=tomorrow&show-quick-entry=true
;; where:
;; - title - the todo
;; - when - ... when :-) accepts dates and time expressions (tomorrow etc)
;; - deadline - similar to when, in terms of format and sets when someting is actually due
;; - show-quick-entry - rather than add the todo, just shows input

;; input format is 'farily' simple:
;; your taks @tomorrow - 'your task' for tomorrow
;; your task @!tomorrow - 'your taks', tomorrow with deadline set to tomorrow
;; your task @tomorrow @!wednesday - 'your task' from tomorrow, needs to be done by wednesday
;; your task @'in 3 days' - your task, scheduled in 3 days
;; your task @!'in 7 years' - your task, with a deadline in 7 years

(defn urlencode [s]
  (-> s
      (URLEncoder/encode "UTF-8")
      ;; WTF JDK
      (str/replace "+" "%20")))

(def when-regex #"(?isuU)(@[\w\-]+)")
(def when-expr-regex #"(?isuU)(@'[\w\-\s]+')")

(def deadline-regex #"(?isuU)(@![\w\-]+)")
(def deadline-expr-regex #"(?isuU)(@!'[\w\-\s]+')")

(defn parse [input]
  (let [;; @tomorrow etc
        whens (mapv (fn [w]
                      {:original w
                       :when (str/replace w "@" "")})
                    (distinct (re-find when-regex (str input))))
        ;; @'in 3 days'
        whens-exprs (mapv (fn [w]
                            {:original w
                             :when (str/replace w #"@'(.+)'" "$1")})
                          (distinct (re-find when-expr-regex (str input))))
        ;; @!tomorrow
        deadlines (mapv (fn [d]
                          {:original d :deadline (str/replace d "@!" "")})
                        (distinct (re-find deadline-regex input)))
        ;; @!'in a week'
        deadlines-exprs (mapv (fn [d]
                                {:original d
                                 :deadline (str/replace d #"@!'(.+)'" "$1")})
                              (distinct (re-find deadline-expr-regex (str input))))
        ;; all strings to remove
        to-remove (remove nil?
                          (concat (map :original whens)
                                  (map :original whens-exprs)
                                  (map :original deadlines)
                                  (map :original deadlines-exprs)))
        ;; strip time expressions
        todo (reduce (fn [i t]
                       (str/replace i t ""))
                     input
                     to-remove)]

    {:todo (str/trim todo)
     :when (first
            (remove nil?
                    (concat (map :when whens-exprs)
                            (map :when whens))))
     :deadline (first
                (remove nil?
                        (concat
                         (map :deadline deadlines)
                         (map :deadline deadlines-exprs))))}))

(defn main [input]
  (let [{:keys [todo deadline] :as parsed} (parse input)
        _ (when (str/blank? todo)
            (throw (ex-info "need a todo!" {:input input})))
        bits
        (cond-> [(format "title=%s" (urlencode todo))]
          (:when parsed) (conj (format "when=%s" (urlencode (:when parsed))))
          deadline (conj (format "deadline=%s" (urlencode deadline)))
          ;; if only deadline, add when to today
          (and deadline
               (not (:when parsed))) (conj "when=today"))]
    (format "things:///add?show-quick-entry=true&%s" (str/join "&" bits))))

(when (seq *command-line-args*)
  (let [input (first *command-line-args*)
        url (main input)]
    (println url)))
