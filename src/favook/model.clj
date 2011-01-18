(ns favook.model
  (:use [favook.util])
  (:require [appengine-magic.services.datastore :as ds])
  )

;; Constants
(def *mail-domain* "@favook.appspotmail.com")
(def *default-limit* 10)

;; Entity Definitions
(ds/defentity User [^:key name avatar secret-mail point date])
(ds/defentity Book [^:key title author isbn point])
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

(defn- add-point [entity]
  (ds/save! (assoc entity :point (inc (:point entity))))
  )

(defn- if-not-today-add-point [query-result else-part-fn]
  (aif (first query-result)
       (when-not (today? (:date it))
         (ds/save! (assoc it :point (inc (:point it)) :date (today)))
         )
       (else-part-fn)
       )
  )

(defn- get-entity-list [kind key val & {:keys [limit page], :or {limit *default-limit* page 1}}]
  (ds/query :kind kind :filter (= key val) :sort [[:date :desc]] :limit limit :offset (* limit (dec page)))
  )

; }}}

;; User
(defn create-user [name avatar]
  (aif (ds/retrieve User name) it
       (let [user (User. name "" (new-secret-mailaddress) 0 (now))]
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
       (let [book (Book. title author isbn 0)]
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
  (ds/save! (Activity. book user message (now))))
(def get-activity-list (partial get-entity-list Activity))

;; Like Book
(defn like-book [#^Book book, #^User user]
  (if-not-today-add-point
    (ds/query :kind LikeBook :filter [(= :book book) (= :user user)])
    #(ds/save! (LikeBook. book user 1 (today)))
    )
  (create-activity book user "like")
  (add-point book)
  )

(defn get-like-book-list [#^User user & {:keys [limit page], :or {limit *default-limit* page 1}}]
  (ds/query :kind LikeBook :filter (= :user user) :sort [:point] :limit limit :offset (* limit (dec page)))
  )

;; LikeUser
(defn like-user [#^User to-user, #^User from-user]
  (when-not (= to-user from-user)
    (if-not-today-add-point
      (ds/query :kind LikeUser :filter [(= :to-user to-user) (= :from-user from-user)])
      #(ds/save! (LikeUser. to-user from-user 1 (today)))
      )
    (add-point to-user)
    )
  )

;(defn get-like-user-list [& {:keys [user total? limit page], :or {user nil, total? false, limit *default-limit*, page 1}}]
;  (let [offset (* limit (dec page))]
;    (cond
;      user (ds/query :kind LikeUser :filter (= :from-user user) :sort [:point] :limit limit :offset offset)
;      total?  (take limit (sort #(> (:point %) (:point %2)) (map (fn [l]
;                                                       (reduce (fn [res x] (assoc res :point (+ (:point res) (:point x)))) l)
;                                                       ) (group-by :to-user (ds/query :kind LikeUser)))))
;      :else (ds/query :kind LikeUser :sort [:point] :limit limit :offset offset)
;      )
;    )
;  )

;; Comment
(defn create-comment [#^Book book, #^User user, text]
  (ds/save! (Comment. book user text (now)))
  (create-activity book user "comment"))
(def get-comment-list (partial get-entity-list Comment))
