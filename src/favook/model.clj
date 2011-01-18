(ns favook.model
  (:use [favook.util])
  (:require [appengine-magic.services.datastore :as ds])
  )

;; Constants
(def *mail-domain* "@favook.appspotmail.com")

;; Entity Definitions
(ds/defentity User [^:key name avatar secret-mail date])
(ds/defentity Book [^:key title author isbn])
(ds/defentity LikeBook [book user point date])
(ds/defentity LikeUser [to-user from-user point date])
(ds/defentity Comment [book user text date])
(ds/defentity Activity [book user message date])

;; Private {{{
(def char-list (concat (range 48 58) (range 65 91) (range 97 123)))
(defn- rand-str [n] (apply str (map char (repeatedly n #(rand-nth char-list)))))

(defn- new-secret-mailaddress []
  (let [mail-address (str (rand-str 16) *mail-domain*)]
    (if (zero? (ds/query :kind User :filter (= :secret-mail mail-address) :count-only? true))
      mail-address
      (new-secret-mailaddress)
      )
    )
  )

(defn- if-not-today-add-point [query-result else-part-fn]
  (aif (first query-result)
       (when-not (today? (:date it))
         (ds/save! (assoc it :point (inc (:point it)) :date (today)))
         )
       (else-part-fn)
       )
  )
; }}}

;; User
(defn create-user [name avatar]
  (aif (ds/retrieve User name) it
       (let [user (User. name "" (new-secret-mailaddress) (today))]
         (ds/save! user)
         user
         )
       )
  )

(defn reset-user-secret-mail [#^User user]
  (ds/save! (assoc user :secret-mail (new-secret-mailaddress)))
  )

(defn get-user [{name :name, :as arg}]
  (ds/retrieve User (if (string? arg) arg name))
  )

;; Book
(defn create-book [title author isbn]
  (aif (ds/retrieve Book title) it
       (let [book (Book. title author isbn)]
         (ds/save! book)
         book
         )
       )
  )
(defn get-book [{title :title, :as arg}]
  (ds/retrieve Book (if (string? arg) arg title))
  )

;; Activity
(defn create-activity [#^Book book, #^User user, message]
  (ds/save! (Activity. book user message (now)))
  )

(defn find-activity [key val & {:keys [limit page], :or {limit 10 page 1}}]
  (ds/query :kind Activity :filter (= key val) :sort [[:date :desc]] :limit limit :offset (* limit (dec page)))
  )

;; Like Book
(defn like-book [#^Book book, #^User user]
  (if-not-today-add-point
    (ds/query :kind LikeBook :filter [(= :book book) (= :user user)])
    #(ds/save! (LikeBook. book user 1 (today)))
    )
  (create-activity book user "like")
  )

(defn get-like-book-list [#^User user & {:keys [limit page], :or {limit 10 page 1}}]
  (ds/query :kind LikeBook :filter (= :user user) :sort [:point] :limit limit :offset (* limit (dec page)))
  )

;; LikeUser
(defn like-user [#^User to-user, #^User from-user]
  (if-not-today-add-point
    (ds/query :kind LikeUser :filter [(= :to-user to-user) (= :from-user from-user)])
    #(ds/save! (LikeUser. to-user from-user 1 (today)))
    )
  )

;; Comment
(defn create-comment [#^Book book, #^User user, text]
  (ds/save! (Comment. book user text (now)))
  (create-activity book user "comment")
  )

