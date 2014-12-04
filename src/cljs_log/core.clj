(ns cljs-log.core
  (:require [cljs.analyzer :as ana]
            [cljs.env]
            [clojure.string :as str]))

(def log-levels
  {:trace 1000
   :debug 2000
   :info 3000
   :warn 4000
   :error 5000
   :fatal 6000})

(defonce levels
  (atom (merge log-levels {:all 0 :none Integer/MAX_VALUE})))

(defonce dev-config (atom {:global :all}))

(defonce prod-config (atom {:global :none}))

(def ^:dynamic *config* nil)

(defmacro register-level! [level value]
  (swap! levels assoc level value))

(defn- swap-config! [f args]
  (let [config (or *config* dev-config)]
    (loop []
      (let [cur @config
            next (apply f cur args)]
        (if (compare-and-set! config cur)
          (when (not= cur next)
            (swap! config dissoc :enable-cache))
          (recur))))))

(defmacro set-global-level!
  [level]
  (swap-config! assoc :global level))

(defmacro set-ns-level!
  ([level] (swap-config! assoc ana/*cljs-ns* level))
  ([ns level] (swap-config! assoc ns level)))

(defmacro override-global-level!
  [level]
  (swap-config! swap! assoc :global-override level))

(defmacro override-ns-level!
  ([level])
  ([ns level]
   (swap-config! assoc-in [:overrides ns] level)))

(defmacro for-production [& body])

(defn- get-active-config [] dev-config)

(def ns-parts
  (memoize
   (fn [nsp]
     (let [ns-parts (str/split (name nsp) #"\.")]
       (for [i (range (count ns-parts))]
         (str/join "." (take (inc i) ns-parts)))))))

(defn- get-logger [] ana/*cljs-ns*)

(defn- is-enabled?
  ([level]
   (let [logger (get-logger)
         active-config (get-active-config)
         {:keys [enable-cache] :as config} @active-config]
     (if-let [enabled? (get enable-cache [logger level])]
       enabled?
       (let [{:keys [global global-override overrides]} config
             parts (ns-parts logger)
             enabled?
             (or
              (>= level global-override)

              (loop [[part & parts] parts]
                (when part
                  (if (when-let [p (get overrides part)]
                        (>= p level))
                    true
                    (recur parts))))

              (and
               (>= level global)
               (loop [[part & parts] parts]
                 (if part
                   (let [p (get config part)]
                     (if (or (not p) (>= p level))
                       (recur parts)
                       false))
                   true))))]
         (swap! active-config assoc-in [:enable-cache [logger level] enabled?])
         enabled?)))))

(defn- get-log-level [level]
  (condp >= (or (get @levels level) level)
    1000 :trace
    2000 :debug
    3000 :info
    4000 :warn
    5000 :error
    :fatal))

(defmacro log [level & args]
  (when (is-enabled? level)
    `(cljs-log.core/print-log ~(get-logger) ~(get-log-level level) ~@args)))

(defmacro trace [& args] `(cljs-log.core/log :trace ~@args))
(defmacro debug [& args] `(cljs-log.core/log :debug ~@args))
(defmacro info [& args] `(cljs-log.core/log :info ~@args))
(defmacro warn [& args] `(cljs-log.core/log :warn ~@args))
(defmacro error [& args] `(cljs-log.core/log :error ~@args))
(defmacro fatal [& args] `(cljs-log.core/log :fatal ~@args))

(defn- cond-macro [default-level body macro-fn]
  (let [level? (first body)
        level (when (keyword? level?) level?)
        gname (if level (second body) level?)
        body (if level (nnext body) (next body))
        level (or level default-level)]
    (when (is-enabled? level)
      (macro-fn level name body))))

(defmacro with-group [& body]
  (cond-macro
   :info body
   (fn [level name body]
     `(do
        (.group js/console ~name)
        ~@body
        (.groupEnd js/console)))))

(defmacro with-group-collapsed [& body]
  (cond-macro
   :info body
   (fn [level name body]
     `(do
        (.groupCollapsed js/console ~name)
        ~@body
        (.groupEnd js/console)))))

(defmacro with-time [& body]
  (cond-macro
   :info body
   (fn [level name body]
     `(do
        (.time js/console ~name)
        ~@body
        (.timeEnd js/console ~name)))))

(defmacro with-profile [& body]
  (cond-macro
   :info body
   (fn [level name body]
     `(do
        (.profile js/console ~name)
        ~@body
        (.profileEnd js/console)))))

;; (defmacro pst [])
