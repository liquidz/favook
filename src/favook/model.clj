(ns favook.model
  (:use
     favook.util
     ds-util
     )
  (:require
     [appengine-magic.services.datastore :as ds]
     )
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

(defn- if-not-today-add-point [query-result else-part-fn]
  (aif (first query-result)
       (when-not (today? (:date it))
         (ds/save! (assoc it :point (inc (:point it)) :date (now)))
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
       (let [user (User. name avatar (new-secret-mailaddress) 0 (now))]
         (ds/save! user)
         user
         )
       )
  )

(defn reset-user-secret-mail [#^User user]
  (ds/save! (assoc user :secret-mail (new-secret-mailaddress)))
  )

(defn get-user [{name :name, :as arg}]
  (ds/retrieve User (if (or (string? arg) (key? arg)) arg name))
  )

(defn get-user-list [& {:keys [limit page], :or {limit *default-limit*, page 1}}]
  (ds/query :kind User :sort [[:point :desc]] :limit limit :offset (* limit (dec page)))
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
  (ds/retrieve Book (if (or (string? arg) (key? arg)) arg title))
  )

(defn get-book-list [& {:keys [limit page], :or {limit *default-limit*, page 1}}]
  (ds/query :kind Book :sort [[:point :desc]] :limit limit :offset (* limit (dec page)))
  )

(defn find-book [key val & {:keys [limit page], :or {limit *default-limit*, page 1}}]
  (let [all-books (ds/query :kind Book :sort [[:point :desc]])
        offset (* limit (dec page))]
    (take limit (drop offset (filter #(not= -1 (.indexOf (key %) val)) all-books)))
    )
  )

(defn str-comp [f s1 s2] (f (.compareTo s1 s2) 0))

;; Activity
(defn create-activity [#^Book book, #^User user, message]
  (ds/save! (Activity. book user message (now))))
(def get-activity-list (partial get-entity-list Activity))

(defn aggregate-activity [message days]
  (let [day-range (n-days-ago (dec days))]
    (->>
      (ds/query :kind Activity :filter (= :message message))
      (filter #(str-comp >= (time->day (:date %)) day-range))
      (group-by :book)
      vals
      (map #(assoc (first %) :point (count %)))
      (sort #(> (:point %1) (:point %2)))
      (map #(dissoc % :date))
      )
    )
  )

;; Like Book
(defn like-book [#^Book book, #^User user]
  (if-not-today-add-point
    (ds/query :kind LikeBook :filter [(= :book book) (= :user user)])
    #(ds/save! (LikeBook. book user 1 (now)))
    )
  (create-activity book user "like")

  ; add point to book
  (let [book* (ds/retrieve Book (ds/get-key-object book))]
    (ds/save! (assoc book* :point (inc (:point book*))))
    )
  )

(defn get-like-book-list [& {:keys [user book limit page], :or {limit *default-limit*, page 1}}]
  (let [offset (* limit (dec page))
        [fkey fval] (if user [:user user] [:book book])]
    (if (or user book)
      (ds/query :kind LikeBook :filter (= fkey fval) :sort [[:point :desc]] :limit limit :offset offset)
      (ds/query :kind LikeBook :sort [[:point :desc]] :limit limit :offset offset)
      )
    )
  )

;; LikeUser
(defn like-user [#^User to-user, #^User from-user]
  (when-not (= to-user from-user)
    (if-not-today-add-point
      (ds/query :kind LikeUser :filter [(= :to-user to-user) (= :from-user from-user)])
      #(ds/save! (LikeUser. to-user from-user 1 (now)))
      )

    ; add point to user
    (let [user* (ds/retrieve User (ds/get-key-object to-user))]
      (ds/save! (assoc user* :point (inc (:point user*))))
      )
    )
  )

(defn get-like-user-list [& {:keys [to-user from-user limit page], :or {limit *default-limit*, page 1}}]
  (let [offset (* limit (dec page))
        [fkey fval] (if to-user [:to-user to-user] [:from-user from-user])]
    (if (or to-user from-user)
      (ds/query :kind LikeUser :filter (= fkey fval) :sort [[:point :desc]] :limit limit :offset offset)
      (ds/query :kind LikeUser :sort [[:point :desc]] :limit limit :offset offset)
      )
    )
  )

;; Comment
(defn create-comment [#^Book book, #^User user, text]
  (ds/save! (Comment. book user text (now)))
  (create-activity book user "comment"))
(def get-comment-list (partial get-entity-list Comment))
