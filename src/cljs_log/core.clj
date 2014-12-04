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

(defn- get-config []
  (or *config* dev-config))

(defmacro set-global-level!
  [level]
  (swap! (get-config) assoc :global level))

(defmacro set-ns-level!
  ([level] (set-ns-level! ana/*cljs-ns* level))
  ([ns level]
   (swap! (get-config) assoc ns level)))

(defmacro override-global-level!
  [level]
  (swap! (get-config) swap! assoc :global-override level))

(defmacro override-ns-level!
  ([level])
  ([ns level]
   (swap! (get-config) assoc-in [:overrides ns] level)))

(defmacro for-production [& body])

(defn- get-active-config [] @dev-config)

(def ns-parts
  (memoize
   (fn [nsp]
     (let [ns-parts (str/split (name nsp) #"\.")]
       (for [i (range (count ns-parts))]
         (str/join "." (take (inc i) ns-parts)))))))

(defn- get-logger [] ana/*cljs-ns*)

(defn- is-enabled?
  ([level]
   (let [{:keys [global global-override overrides] :as config} (get-active-config)
         logger (get-logger) 
         parts (ns-parts logger)]
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
           true)))))))

(defn- get-log-level [level]
  (condp >= (or (get @levels level) level)
    1000 :trace
    2000 :debug
    3000 :info
    4000 :warn
    5000 :error
    :fatal))

(defmacro log [level & args]
  `(cljs-log.core/print-log ~(get-logger) ~(get-log-level level) ~@args))
