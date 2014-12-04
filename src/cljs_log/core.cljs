(ns cljs-log.core)

(def format-msg
  (fn [logger level msg]
    (str level " " logger " - " msg)))

(defn output-log [level msg]
  ())

(defn print-log [logger level & args]
  (let [msg (apply print-str args)
        msg (format-msg logger level msg)]
    (output-log level msg)))

