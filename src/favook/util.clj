(ns favook.util
  (:use favook.constants ds-util
     )
  (:require [clojure.contrib.string :as string]
     [clojure.contrib.json :as json]
     [appengine-magic.services.datastore :as ds]
     )
  (:import
     [java.util TimeZone Calendar]
     [java.security MessageDigest]
     )
  )

(defmacro aif [expr then & [else]]
  `(let [~'it ~expr] (if ~'it ~then ~else))
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

(defn aggregate-entity-points [group-keyword entity-list]
  (sort
    #(> (:point %1) (:point %2))
    (map #(reduce (fn [res x] (assoc res :point (+ (:point res) (:point x)))) %)
         (vals (group-by group-keyword entity-list))))
  )

(defn params->limit-and-page [params]
  (let [limit (aif (:limit params) (parse-int it) *default-limit*)
        page (aif (:page params) (parse-int it) 1)]
    [(if (pos? limit) limit *default-limit*)
     (if (pos? page) page 1)]))

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

(defn guest? [user]
  (and
    (= *guest-name* (:name user))
    (= *guest-avatar* (:avatar user))
    (= *guest-mail* (:secret-mail user))
    )
  )


(defn bytes->hex-str [bytes]
  (apply str (map #(string/tail 2 (str "0" (Integer/toHexString (bit-and 0xff %)))) bytes))
  )
(defn digest-hex [algorithm s]
  (if-not (string/blank? s)
    (-> (MessageDigest/getInstance algorithm) (.digest (.getBytes s)) bytes->hex-str)
    )
  )
;(def str->md5 (partial digest-hex "MD5"))
(def str->sha1 (partial digest-hex "SHA1"))

(defn make-book-user-key [#^Book book, #^User user]
  (str->sha1 (str (:title book) (:author book) (:isbn book) (:name user) (:avatar user)))
  )

(defn make-user-user-key [#^User user1, #^User user2]
  (str->sha1 (str (:name user1) (:avatar user1) (:name user2) (:avatar user2)))
  )

(defn println* [& args]
  (apply println args)
  (last args)
  )

(defn isbn? [s]
  (let [s* (string/replace-str "-" "" s)]
    (and (not (nil? (re-matches #"[0-9]+" s*)))
         (or (= 10 (count s*)) (= 13 (count s*)))
         )
    )
  )
