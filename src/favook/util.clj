(ns favook.util
  (:use favook.constants ds-util)
  (:require [clojure.contrib.string :as string]
     [clojure.contrib.json :as json]
     )
  (:import [java.util TimeZone Calendar])
  )

(defmacro aif [expr then & [else]]
  `(let [~'it ~expr] (if ~'it ~then ~else))
  )

(defn remove-extra-key [m] (dissoc m :secret-mail))

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

(defn- json-conv [obj]
  (cond
    (or (seq? obj) (list? obj)) (map json-conv obj)
    (map? obj) (map-val-map json-conv (remove-extra-key obj))
    (key? obj) (key->str obj)
    :else obj
    )
  )
(defn to-json [obj] (json/json-str (json-conv obj)))

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

(defn aggregate-entity-points [group-keyword entity-list]
  (sort
    #(> (:point %1) (:point %2))
    (map #(reduce (fn [res x] (assoc res :point (+ (:point res) (:point x)))) %)
         (vals (group-by group-keyword entity-list))))
  )

(defn params->limit-and-page [params]
  [(aif (:limit params) (parse-int it) *default-limit*)
   (aif (:page params) (parse-int it) 1)])

(defn take-if
  ([f col] (filter f col))
  ([n f col]
   (loop [ls col, i 0, res ()]
     (if (or (empty? ls) (= i n))
       (reverse res)
       (if (-> ls first f)
         (recur (rest ls) (inc i) (cons (first ls) res))
         (recur (rest ls) i res)
         )
       )
     )
   )
  )


(defn default-response [obj]
  (if (map? obj) obj {:status 200 :headers {"Content-Type" "text/html"} :body obj})
  )


(defn with-session
  ([session res m]
   (assoc (default-response res) :session (conj (aif session it {}) m)))
  ([res m] (with-session nil res m))
  )

(defn with-message
  ([session res msg] (with-session session res {:message msg}))
  ([res msg] (with-message nil res msg))
  )

(defn loggedin? [session] (:loggedin session))
(defn login-name [session] (:name session))
(defn login-avatar [session] (:avatar session))

(defn println* [& args]
  (apply println args)
  (last args)
  )

