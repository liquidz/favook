(ns favook.model
  (:use
     [favook constants util]
     ds-util
     )
  (:require
     [appengine-magic.services.datastore :as ds]
     [clojure.contrib.string :as string]
     key
     )

  ;;rakuten
  (:require
     [clojure.contrib.json :as json]
     [clojure.contrib.io :as io])
  )

; rakuten {{{
(def *rakuten-developer-id* nil)
(def *rakuten-affiliate-id* nil)
(def *api-urls*
  {"2009-03-26" "http://api.rakuten.co.jp/rws/2.0/json?"
   "2009-04-15" "http://api.rakuten.co.jp/rws/2.0/json?"
   "2010-03-18" "http://api.rakuten.co.jp/rws/3.0/json?"})
(def *version* "2010-03-18")
(def *operation* "BooksBookSearch")

(defmacro with-rakuten-developer-id [dev-id & body]
  `(binding [*rakuten-developer-id* ~dev-id] ~@body))
(defmacro with-version [version & body]
  `(binding [*version* ~version] ~@body))
(defmacro with-affiliate-id [aff-id & body]
  `(binding [*rakuten-affiliate-id* ~aff-id] ~@body))

(defn- map->url-parameters [m]
  (apply str (interpose "&" (map (fn [[k v]] (str (name k) "=" v)) m)))
  )

(defn- make-url [m]
  (let [data (assoc m :developerId *rakuten-developer-id* :operation *operation* :version *version*)
        data2 (if (nil? *rakuten-affiliate-id*) data (assoc data :affiliateId *rakuten-affiliate-id*))
        ]
    (str (get *api-urls* *version*) (map->url-parameters data2))
    )
  )

(defn rakuten-book-search [& {:as m}]
  (json/read-json (apply str (io/read-lines (make-url m))))
  )
; }}}


;; Entity Definitions
(ds/defentity User [^:key name avatar secret-mail point date])
(ds/defentity Book [^:key title author isbn thumbnail point])
(ds/defentity LikeBook [^:key id book user point date])
(ds/defentity LikeUser [^:key id to-user from-user point date])
(ds/defentity Comment [^:key id book user text date])
(ds/defentity Activity [^:key id book user message date])

(declare get-user get-book get-like-book)

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

(defn- get-entity-list [kind key val & {:keys [limit page], :or {limit *default-limit* page 1}}]
  (ds/query :kind kind :filter (= key val) :sort [[:date :desc]] :limit limit :offset (* limit (dec page)))
  )

(defn- add-can-like [#^LikeBook lb]
  (if (nil? lb) lb (assoc lb :likeyet false :canlike (-> lb :date today? not)))
  )

(defn- add-like-data [#^Book book, #^User user]
  (when-not (or (nil? book) (nil? user))
    (if (guest? user)
      (assoc book :likeyet true :canlike false)
      (let [like-book-entity (get-like-book book user)]
        (assoc book
               :likeyet (nil? like-book-entity)
               :canlike (or (nil? like-book-entity) (-> like-book-entity :date today? not))
               )
        )
      )
    )
  )

(defn fill-book-info [{:keys [title author isbn], :as book}]
  (with-rakuten-developer-id
    key/*rakuten-developer-id*
    (let [bookdata (if (string/blank? isbn)
                     (rakuten-book-search :title title :author author)
                     (rakuten-book-search :isbn isbn))]
      (when-not (= "NotFound" (-> bookdata :Header :Status))
        (let [info (-> bookdata :Body :BooksBookSearch)
              one? (= 1 (:count info))
              first-item (if one? (-> info :Items :Item first))
              filled-title (if (and one? (or (string/blank? title) (= title *dummy-title*)))
                             (:title first-item) title)
              filled-author (if (and one? (string/blank? author))
                              (:author first-item) author)
              thumbnail (if one?
                          [(:smallImageUrl first-item)
                           (:mediumImageUrl first-item)
                           (:largeImageUrl first-item)])
              filled-isbn (if (and one? (string/blank? isbn))
                            (:isbn first-item) isbn)]

          (assoc book :title filled-title :author filled-author :isbn filled-isbn :thumbnail thumbnail)
          )
        )
      )
    )
  )

; }}}

;(defn key-property->entity [e]
;  (when-not (nil? e)
;    (let [get-user* (fn [e* k] (remove-extra-key (get-user (k e*))))]
;      (case (get-kind e)
;        ("LikeBook" "Activity") (assoc e :user (get-user* e :user) :book (get-book (:book e)))
;        "LikeUser" (assoc e :to-user (get-user* e :to-user) :from-user (get-user* e :from-user))
;        nil
;        )
;      )
;    )
;  )

;; User
(defn create-user [name avatar & {:keys [guest?] :or {guest? false}}]
  (aif (ds/retrieve User name) it
       (let [key (ds/save! (User. name avatar (if guest? *guest-mail* (new-secret-mailaddress)) 0 (now)))]
         (ds/retrieve User key)
         )
       ;(let [user (User. name avatar (if guest? *guest-mail* (new-secret-mailaddress)) 0 (now))]
       ;  (ds/save! user)
       ;  user
       ;  )
       )
  )

(defn reset-user-secret-mail [#^User user]
  (ds/save! (assoc user :secret-mail (new-secret-mailaddress)))
  )

(defn get-user [{:keys [name] :as arg}]
  (ds/retrieve User (if (or (string? arg) (key? arg)) arg name))
  )

(defn get-user-from-mail [mail]
  (first (ds/query :kind User :filter (= :secret-mail mail) :limit 1))
  )

(defn get-user-list [& {:keys [limit page], :or {limit *default-limit*, page 1}}]
  (ds/query :kind User :sort [[:point :desc]] :limit limit :offset (* limit (dec page)))
  )

;; Book
(defn create-book [title author isbn & {:keys [fill?] :or {title *dummy-title* *fill? false}}]
  (when (or (not (string/blank? title))
            (and (string/blank? title) (not (string/blank? isbn)) fill?))
    (let [title* (if (nil? title) *dummy-title* title)]
      (aif (ds/retrieve Book title*) it
           (let [book (Book. title* author isbn nil 0)
                 filled-book (if fill? (fill-book-info book) book)
                 ]
             (if-not (nil? filled-book)
               (ds/retrieve Book (ds/save! filled-book))
               )
             )
           )
      )
    )
  )

(defn get-book [{:keys [title user], :as arg}]
  (let [res (ds/retrieve Book (if (or (string? arg) (key? arg)) arg title))]
    (if (string? user)
      (add-like-data res (ds/retrieve User user))
      res
      )
    )
  )

(defn get-book-list [& {:keys [limit page user all?], :or {limit *default-limit*, page 1, all? false}}]
  (let [user-entity (if user (ds/retrieve User user))
        res (if all?
              (ds/query :kind Book :sort [[:point :desc]])
              (ds/query :kind Book :sort [[:point :desc]] :limit limit :offset (* limit (dec page))))
        ]
    (if user-entity
      (map #(add-like-data % user-entity) res)
      res
      )
    )
  )

(defn find-book [key val & {:keys [limit page], :or {limit *default-limit*, page 1}}]
  (let [all-books (ds/query :kind Book :sort [[:point :desc]])
        offset (* limit (dec page))]
    (take limit (drop offset (filter #(and
                                        (not (nil? (key %)))
                                        (not= -1 (.indexOf (key %) val))) all-books)))
    )
  )

(defn str-comp [f s1 s2] (f (.compareTo s1 s2) 0))

;; Activity
(defn get-activity
  ([#^String key] (ds/retrieve Activity key))
  ([#^Book book, #^User user] (get-activity (make-book-user-key book user)))
  )
(defn create-activity [#^Book book, #^User user, message]
  (ds/retrieve Activity (ds/save! (Activity. (make-book-user-key book user message)
                                             book user message (now))))
  )
(defn get-activity-list [& {:keys [book user message limit page], :or {book nil, user nil, limit *default-limit*, page 1}}]
  (let [offset (* limit (dec page))]
    (if (and (nil? book) (nil? user))
      (if message
        (ds/query :kind Activity :filter (= :message message) :sort [[:date :desc]] :limit limit :offset offset)
        (ds/query :kind Activity :sort [[:date :desc]] :limit limit :offset offset)
        )
      (let [[key val] (if book [:book book] (if user [:user user]))]
        (if message
          (ds/query :kind Activity :filter [(= key val) (= :message message)] :sort [[:date :desc]] :limit limit :offset offset)
          (ds/query :kind Activity :filter (= key val) :sort [[:date :desc]] :limit limit :offset offset)
          )
        )
      )
    )
  )

(defn aggregate-activity [message group-keyword days]
  (let [day-range (n-days-ago (dec days))]
    (when (or (= group-keyword :user) (= group-keyword :book))
      (->>
        (ds/query :kind Activity :filter (= :message message))
        (filter #(str-comp >= (time->day (:date %)) day-range))
        (group-by group-keyword)
        vals
        (map #(assoc (first %) :point (count %)))
        (sort #(> (:point %1) (:point %2)))
        ;(map #(dissoc % :date))
        )
      )
    )
  )

;; Like Book

(defn get-like-book
  ([#^String key] (add-can-like (ds/retrieve LikeBook key)))
  ([#^Book book, #^User user] (get-like-book (make-book-user-key book user)))
  )

(defn like-book [#^Book book, #^User user & {:keys [point], :or {point 1}}]
  (let [key (make-book-user-key book user)]
    (aif (ds/retrieve LikeBook key)
      (when-not (today? (:date it))
        (ds/save! (assoc it :point (inc (:point it)) :date (now)))
        )
      (ds/save!
        (LikeBook.
          key book user
          (if (and (pos? point) (<= point *first-like-point-max*)) point 1)
          (now)))
      )
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
      (let [res (ds/query :kind LikeBook :filter (= fkey fval) :sort [[:point :desc]] :limit limit :offset offset)
            ]
        (if user
          (map add-can-like res)
          res
          )
        )
      (ds/query :kind LikeBook :sort [[:point :desc]] :limit limit :offset offset)
      )
    )
  )

;; LikeUser
(defn get-like-user
  ([#^String key] (ds/retrieve LikeUser key))
  ([#^User user1, #^User user2] (get-like-user (make-user-user-key user1 user2)))
  )

(defn like-user [#^User to-user, #^User from-user]
  (when-not (= to-user from-user)
    (aif (ds/retrieve LikeUser (make-user-user-key to-user from-user))
         (when-not (today? (:date it))
           (ds/save! (assoc it :point (inc (:point it)) :date (now)))
           )
         (ds/save! (LikeUser. (make-user-user-key to-user from-user) to-user from-user 1 (now)))
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
  (let [key (ds/save! (Comment. (make-book-user-key book user) book user text (now)))]
    (create-activity book user "comment")
    (ds/retrieve Comment key)
    )
  )
(def get-comment-list (partial get-entity-list Comment))



;; Test
(defn create-test-data []
  (let [user1 (create-user "aa" *guest-avatar*)
        user2 (create-user "bb" *guest-avatar*)
        user3 (create-user "cc" *guest-avatar*)
        book1 (create-book "hoge" "fuga" "4001156768")
        book2 (create-book "neko" "nyan" "4001156768")
        book3 (create-book "inu" "wan" "4001156768")
        ]
    (like-book book1 user1)
    (like-book book1 user2)
    (like-book book2 user3)

    (like-user user1 user2)
    (like-user user1 user3)
    (like-user user2 user1)

    (create-comment book1 user1 "hello")
    (create-comment book1 user2 "wani")
    (create-comment book2 user3 "tori")
    )
  )

;; Output Utils
(defn remove-extra-key [m] (dissoc m :secret-mail))

(def delete-html-tag (partial string/replace-re #"<.+?>" ""))

(defn key->entity [key]
  (when-not (nil? key)
    (let [get-user* (fn [e* k] (remove-extra-key (get-user (k e*))))]
      (case (.getKind key)
        "User" (remove-extra-key (ds/retrieve User key))
        "Book" (ds/retrieve Book key)
        ;"LikeBook"
        ;(let [e (ds/retrieve LikeBook key)]
        ;  (assoc e :user (get-user* e :user) :book (get-book (:book e))))
        ;"Activity"
        ;(let [e (ds/retrieve Activity key)]
        ;  (assoc e :user (get-user* e :user) :book (get-book (:book e))))
        ;"LikeUser"
        ;(let [e (ds/retrieve LikeUser key)]
        ;  (assoc e :to-user (get-user* e :to-user) :from-user (get-user* e :from-user)))
        nil
        )
      )
    )
  )
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
    (key? obj) (key->entity obj) ;(key->str obj)
    :else obj
    )
  )
(defn to-json [obj] (json/json-str (json-conv obj)))
