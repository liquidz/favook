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

(defn calendar-format
  ([calendar-obj format-str timezone-str]
   (.setTimeZone calendar-obj (TimeZone/getTimeZone timezone-str))
   (format format-str calendar-obj))
  ([calendar-obj format-str] (calendar-format calendar-obj format-str "Asia/Tokyo")))

(def today-calendar-format (partial calendar-format (Calendar/getInstance)))
(def today (partial today-calendar-format "%1$tY/%1$tm/%1$td"))
(def now (partial today-calendar-format "%1$tY/%1$tm/%1$td %1$tH:%1$tM:%1$tS"))

(defn n-days-ago [n]
  (let [cal (Calendar/getInstance)]
    (.add cal Calendar/DATE (* -1 n))
    (calendar-format cal "%1$tY/%1$tm/%1$td")
    )
  )

(defn time->day [s] (first (string/split #"\s+" s)))

(defn today? [date]
  (= (today) (time->day date))
  )

(def parse-int #(Integer/parseInt %))
