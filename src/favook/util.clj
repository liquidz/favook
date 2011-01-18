(ns favook.util
  (:require [clojure.contrib.string :as string])
  (:import [java.util TimeZone Calendar])
  )

(defmacro aif [expr then & [else]]
  `(let [~'it ~expr] (if ~'it ~then ~else))
  )

(def delete-html-tag (partial string/replace-re #"<.+?>" ""))

(defn convert-map [m]
  (apply
    hash-map
    (interleave
      (map keyword (keys m))
      (map (comp string/trim delete-html-tag) (vals m))))
  )

(defn map-val-map [f m]
  (apply hash-map (mapcat (fn [[k v]] [k (f v)]) m))
  )

(defn today
  ([timezone-str]
   (format "%1$tY/%1$tm/%1$td" (Calendar/getInstance (TimeZone/getTimeZone timezone-str)))
   )
  ([] (today "Asia/Tokyo"))
  )
(defn today? [date] (= date (today)))
