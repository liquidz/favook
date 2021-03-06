(ns favook.test.core
  (:use
     [favook core constants model util]
     ds-util
     :reload)
  (:use clojure.test ring.mock.request)

  (:require
     [appengine-magic.testing :as ae-testing]
     [appengine-magic.services.datastore :as ds]
     [clojure.contrib.json :as json]
     [clojure.contrib.string :as string]
     )
  )

(use-fixtures :each (ae-testing/local-services :all))

;; Entity Definitions {{{
(ds/defentity User [^:key name avatar secret-mail point date])
(ds/defentity Book [^:key title author isbn thumbnail point])
(ds/defentity LikeBook [^:key id book user point date])
(ds/defentity LikeUser [^:key id to-user from-user point date])
(ds/defentity Comment [^:key id book user text date])
(ds/defentity Activity [^:key id book user message date])
; }}}

(defn- foreach [f l] (doseq [x l] (f x)))
(defn- body->json [res] (-> res :body json/read-json))
(defn- testGET [& urls] (favook-app-handler (request :get (apply str urls))))
(defn- testGET-with-session [res & urls]
  (let [ring-session (first (get (:headers res) "Set-Cookie"))]
    (favook-app-handler (header (request :get (apply str urls)) "Cookie" ring-session))
    )
  )
(defn- testPOST [data & urls]
  (favook-app-handler (body (request :post (apply str urls)) data)))
(defn- testPOST-with-session [res data & urls]
  (let [ring-session (first (get (:headers res) "Set-Cookie"))]
    (favook-app-handler (header (body (request :post (apply str urls)) data) "Cookie" ring-session))
    )
  )

(defn- update-activity-date [b u d]
  (ds/save! (assoc (ds/retrieve Activity (make-book-user-key b u)) :date d)))

(defn- nth-book-entity [el n]
  (let [book (:book (nth el n))]
    (ds/retrieve Book (if (key? book) book (str->key book)))))
(defn- nth-book [el n] (:book (nth el n)))
;(defn- nth-user [key el n]
;  (let [user (key (nth el n))]
;    (ds/retrieve User (if (key? user) user (str->key user)))))
(defn- nth-user [key el n] (key (nth el n)))
(defn- entity->key-str [e] (key->str (ds/get-key-object e)))

;; == Ds-Util Test ==
(deftest test-entity? ; {{{
  (is (entity? (User. "a" "b" "c" "d" "e")))
  ) ; }}}

;; == Model Test ==
(deftest test-create-user ; {{{
  (create-user "aa" "")
  (is (= 1 (ds/query :kind "User" :count-only? true)))
  (create-user "bb" "")
  (is (= 2 (ds/query :kind "User" :count-only? true)))
  (create-user "aa" "")
  (is (= 2 (ds/query :kind "User" :count-only? true)))
  ); }}}

(deftest test-get-user ; {{{
  (create-user "aa" "")
  (is (= "aa" (:name (get-user "aa"))))
  (is (= "aa" (:name (get-user {:name "aa"}))))
  (is (nil? (get-user "bb")))
  ) ; }}}

(deftest test-get-user-list ; {{{
  (let [user1 (create-user "aa" "") user2 (create-user "bb" "")
        user3 (create-user "cc" "") user4 (create-user "dd" "")]
    (like-user user1 user2) (like-user user1 user3) (like-user user1 user4)
    (like-user user2 user1) (like-user user2 user3) (like-user user3 user1)

    (are [x y] (= x y)
      4 (count (get-user-list))
      2 (count (get-user-list :limit 2))
      "aa" (:name (first (get-user-list)))
      3 (:point (first (get-user-list)))
      "bb" (:name (first (get-user-list :limit 1 :page 2)))
      2 (:point (first (get-user-list :limit 1 :page 2)))
      "dd" (:name (last (get-user-list)))
      0 (:point (last (get-user-list)))
      )
    )
  ) ; }}}

(deftest test-reset-user-secret-mail; {{{
  (let [user (create-user "aa" "")
        old-mail (:secret-mail user)]
    (reset-user-secret-mail user)
    (is (not= old-mail (:secret-mail (ds/retrieve User "aa"))))
    )
  ) ; }}}

(deftest test-create-book ; {{{
  (let [book1 (create-book "aa" "bb" "cc")
        book2 (create-book nil nil "4001156768" :fill? true)
        book3 (create-book nil nil "xxxxxxxxxx" :fill? true)
        ]
    (are [x y] (= x y)
      nil (create-book nil nil nil)
      "aa" (:title book1)
      "bb" (:author book1)
      "cc" (:isbn book1)
      nil (:thumbnail book1)
      0 (:point book1)

      false (string/blank? (:title book2))
      false (string/blank? (:author book2))
      false (nil? (:thumbnail book2))
      false (string/blank? (nth (:thumbnail book2) 0))
      false (string/blank? (nth (:thumbnail book2) 1))
      false (string/blank? (nth (:thumbnail book2) 2))
      0 (:point book2)

      nil book3
      )
    )
  ) ; }}}

(deftest test-get-book ; {{{
  (let [book (create-book "title" "" "")
        user (create-user "aa" "")]

    (are [x y] (= x y)
      "title" (:title (get-book "title"))
      "title" (:title (get-book {:title "title"}))
      nil (get-book "unknown")
      )

    (like-book book user)
    (ds/save! (assoc (get-like-book book user) :date (n-days-ago 1)))
    (are [x y] (= x y)
      false (:likeyet (get-book {:title "title" :user "aa"}))
      true  (:canlike (get-book {:title "title" :user "aa"}))
      )
    )
  ) ; }}}

(deftest test-get-book-list ; {{{
  (let [user1 (create-user "aa" "") user2 (create-user "bb" "") user3 (create-user "cc" "")
        book1 (create-book "hoge" "" "") book2 (create-book "fuga" "" "") book3 (create-book "neko" "" "")]
    ;add points
    (like-book book1 user1) (like-book book1 user2) (like-book book1 user3)
    (like-book book2 user1) (like-book book2 user2)
    (ds/save! (assoc (get-like-book book1 user1) :date (n-days-ago 1)))

    (are [x y] (= x y)
      3 (count (get-book-list))
      2 (count (get-book-list :limit 2))
      3 (count (get-book-list :all? true))
      3 (count (get-book-list :limit 1 :all? true))
      "hoge" (:title (first (get-book-list)))
      3 (:point (first (get-book-list)))
      "fuga" (:title (first (get-book-list :limit 1 :page 2)))
      2 (:point (first (get-book-list :limit 1 :page 2)))
      "neko" (:title (last (get-book-list)))
      0 (:point (last (get-book-list)))

      false (:likeyet (first (get-book-list :user "aa")))
      false (:likeyet (second (get-book-list :user "aa")))
      true  (:likeyet (nth (get-book-list :user "aa") 2))
      false (:likeyet (first (get-book-list :user "bb")))
      false (:likeyet (second (get-book-list :user "bb")))
      true  (:likeyet (nth (get-book-list :user "bb") 2))
      false (:likeyet (first (get-book-list :user "cc")))
      true  (:likeyet (second (get-book-list :user "cc")))
      true  (:likeyet (nth (get-book-list :user "cc") 2))

      true  (:canlike (first (get-book-list :user "aa")))
      false (:canlike (second (get-book-list :user "aa")))
      true  (:canlike (nth (get-book-list :user "aa") 2))
      )
    )
  ) ; }}}

(deftest test-find-book ; {{{
  (create-book "hello" "aa" "") (ds/save! (assoc (ds/retrieve Book "hello") :point 3))
  (create-book "neko" "bb" nil) (ds/save! (assoc (ds/retrieve Book "neko") :point 2))
  (create-book "inu" "bb" "") (ds/save! (assoc (ds/retrieve Book "inu") :point 1))
  (create-book "wani" "cc" "test")

  (are [x y] (= x y)
    0 (count (find-book :title "unknown"))
    1 (count (find-book :title "hello"))
    2 (count (find-book :title "e"))
    1 (count (find-book :title "e" :limit 1))
    "hello" (:title (first (find-book :title "e")))
    "neko" (:title (first (find-book :title "e" :limit 1 :page 2)))

    0 (count (find-book :author "unknown"))
    2 (count (find-book :author "bb"))
    1 (count (find-book :author "bb" :limit 1))
    "neko" (:title (first (find-book :author "bb")))
    "inu" (:title (first (find-book :author "bb" :limit 1 :page 2)))

    1 (count (find-book :isbn "test"))
    )
  ) ; }}}

(deftest test-like-book ; {{{
  (let [user1 (create-user "aa" "") user2 (create-user "bb" "")
        book1 (create-book "title" "" "") book2 (create-book "fuga" "" "")]
    ;(ds/save! [user1 user2 book])

    (like-book book1 user1)
    (is (= 1 (ds/query :kind LikeBook :count-only? true)))
    (like-book book1 user2)
    (is (= 2 (ds/query :kind LikeBook :count-only? true)))
    (like-book book1 user1)
    (is (= 2 (ds/query :kind LikeBook :count-only? true)))
    (let [like (first (ds/query :kind LikeBook :filter (= :user user1)))]
      (is (= 1 (:point like)))
      (ds/save! (assoc like :date "1900-01-01"))
      (like-book book1 user1)
      (is (= 2 (:point (first (ds/query :kind LikeBook :filter (= :user user1))))))
      )

    (like-book book2 user1 :point 2)
    (let [lb (ds/retrieve LikeBook (make-book-user-key book2 user1))]
      (is (= 2 (:point (ds/retrieve LikeBook (make-book-user-key book2 user1)))))
      (like-book book2 user1 :point 3)
      (is (= 2 (:point (ds/retrieve LikeBook (make-book-user-key book2 user1)))))
      (ds/save! (assoc (ds/retrieve LikeBook (make-book-user-key book2 user1)) :date (n-days-ago 1)))
      (like-book book2 user1 :point 3)
      (is (= 3 (:point (ds/retrieve LikeBook (make-book-user-key book2 user1)))))
      (like-book book2 user1)
      (is (= 3 (:point (ds/retrieve LikeBook (make-book-user-key book2 user1)))))
      )
    )
  ) ; }}}

(deftest test-get-like-book ; {{{
  (let [user1 (create-user "aa" "") user2 (create-user "bb" "")
        book1 (create-book "hoge" "" "")
        key1 (make-book-user-key book1 user1)
        key2 (make-book-user-key book1 user2)]
    (like-book book1 user1)
    (ds/save! (assoc (first (ds/query :kind LikeBook :filter [(= :book book1) (= :user user1)])) :date (n-days-ago 1)))

    (are [x y] (= x y)
      false (nil? (get-like-book book1 user1))
      nil (get-like-book book1 user2)
      false (nil? (get-like-book key1))
      nil (get-like-book key2)
      "hoge" (:title (ds/retrieve Book (:book (get-like-book book1 user1))))
      "aa" (:name (ds/retrieve User (:user (get-like-book book1 user1))))

      false (:likeyet (get-like-book book1 user1))
      true (:canlike (get-like-book book1 user1))
      nil (:likeyet (get-like-book book1 user2))
      nil (:canlike (get-like-book book1 user2))
      )
    )
  ) ; }}}

(deftest test-get-like-book-list ; {{{
  (let [user1 (create-user "aa" "") user2 (create-user "bb" "") user3 (create-user "cc" "")
        book1 (create-book "hoge" "" "") book2 (create-book "fuga" "" "") book3 (create-book "neko" "" "")]
    (like-book book1 user1) (like-book book2 user1) (like-book book3 user1)
    (ds/save! (assoc (get-like-book book3 user1) :point 3 :date (n-days-ago 1)))
    (ds/save! (assoc (get-like-book book2 user1) :point 2))
    (like-book book1 user2) (like-book book3 user2)

    (are [x y] (= x y)
      5 (count (get-like-book-list))
      3 (count (get-like-book-list :limit 3))

      3 (count (get-like-book-list :user user1))
      2 (count (get-like-book-list :user user2))
      0 (count (get-like-book-list :user user3))
      1 (count (get-like-book-list :user user1 :limit 1))
      "neko" (:title (ds/retrieve Book (:book (first (get-like-book-list :user user1 :limit 1)))))
      "fuga" (:title (ds/retrieve Book (:book (first (get-like-book-list :user user1 :limit 1 :page 2)))))

      2 (count (get-like-book-list :book book1))
      1 (count (get-like-book-list :book book1 :limit 1))
      1 (count (get-like-book-list :book book2))
      "aa" (:name (ds/retrieve User (:user (first (get-like-book-list :book book2)))))

      false (:likeyet (first (get-like-book-list :user user1)))
      true (:canlike (first (get-like-book-list :user user1)))
      false (:likeyet (second (get-like-book-list :user user1)))
      false (:canlike (second (get-like-book-list :user user1)))
      false (:likeyet (second (get-like-book-list :user user2)))
      false (:canlike (second (get-like-book-list :user user2)))
      )
    )
  ) ; }}}

(deftest test-like-user ; {{{
  (let [user1 (create-user "aa" "") user2 (create-user "bb" "")]
    (like-user user1 user1)
    (is (zero? (ds/query :kind LikeUser :count-only? true)))
    (like-user user1 user2)
    (is (= 1 (ds/query :kind LikeUser :count-only? true)))
    (like-user user2 user1)
    (is (= 2 (ds/query :kind LikeUser :count-only? true)))
    (like-user user1 user2)
    (is (= 2 (ds/query :kind LikeUser :count-only? true)))
    (let [like (first (ds/query :kind LikeUser :filter (= :to-user user1)))]
      (is (= 1 (:point like)))
      (ds/save! (assoc like :date "1900-01-01"))
      (like-user user1 user2)
      (is (= 2 (:point (first (ds/query :kind LikeUser :filter (= :to-user user1))))))
      )
    )
  ) ; }}}

(deftest test-get-like-user ; {{{
  (let [user1 (create-user "aa" "")
        user2 (create-user "bb" "")
        user3 (create-user "cc" "")
        key1 (make-user-user-key user1 user2)
        key2 (make-user-user-key user1 user3)
        ]
    (like-user user1 user2)

    (are [x y] (= x y)
      false (nil? (get-like-user user1 user2))
      nil (get-like-user user1 user3)
      false (nil? (get-like-user key1))
      nil (get-like-user key2)
      )
    )
  ) ; }}}

(deftest test-get-like-user-list ; {{{
  (let [user1 (create-user "aa" "")
        user2 (create-user "bb" "")
        user3 (create-user "cc" "")
        ]
    (like-user user2 user1)
    (ds/save! (assoc (get-like-user user2 user1) :point 3))
    (like-user user3 user1)
    (ds/save! (assoc (get-like-user user3 user1) :point 2))
    (like-user user3 user2)

    (are [x y] (= x y)
      3 (count (get-like-user-list))
      2 (count (get-like-user-list :limit 2))

      2 (count (get-like-user-list :from-user user1))
      1 (count (get-like-user-list :from-user user1 :limit 1))
      3 (:point (first (get-like-user-list :from-user user1)))
      "bb" (:name (ds/retrieve User (:to-user (first (get-like-user-list :from-user user1)))))

      2 (count (get-like-user-list :to-user user3))
      1 (count (get-like-user-list :to-user user3 :limit 1))
      "aa" (:name (ds/retrieve User (:from-user (first (get-like-user-list :to-user user3)))))
      )
    )
  ) ; }}}

(deftest test-get-activity-list ; {{{
  (let [user (create-user "aa" "") book1 (create-book "title" "" "") book2 (create-book "hoge" "" "")]
    (like-book book1 user) (create-comment book2 user "hello")
    (ds/save! (assoc (get-activity book1 user) :date "1900-01-01"))

    (are [x y] (= x y)
      2 (count (get-activity-list))
      "comment" (:message (first (get-activity-list)))

      2 (count (get-activity-list :user user))
      1 (count (get-activity-list :user user :limit 1))
      1 (count (get-activity-list :book book1))

      1 (count (get-activity-list :user user :message "like"))
      "title" (:title (ds/retrieve Book (:book (first (get-activity-list :user user :message "like")))))
      1 (count (get-activity-list :user user :message "comment"))
      "hoge" (:title (ds/retrieve Book (:book (first (get-activity-list :user user :message "comment")))))

      "hoge" (:title (ds/retrieve Book (:book (first (get-activity-list :user user)))))
      "title" (:title (ds/retrieve Book (:book (first (get-activity-list :user user :limit 1 :page 2)))))

      "aa" (:name (ds/retrieve User (:user (first (get-activity-list :book book1)))))
      "like" (:message (first (get-activity-list :book book1)))
      "comment" (:message (first (get-activity-list :book book2)))
      )
    )
  ) ; }}}

(deftest test-aggregate-activity ; {{{
  (let [user1 (create-user "aa" "") user2 (create-user "bb" "")
        book1 (create-book "hoge" "" "") book2 (create-book "fuga" "" "")]

    (like-book book1 user1) (update-activity-date book1 user1 (n-days-ago 1))
    (like-book book2 user1)
    (like-book book1 user2) (update-activity-date book1 user2 (n-days-ago 2))

    (are [x y] (= x y)
      1 (count (aggregate-activity "like" :book 1))
      2 (count (aggregate-activity "like" :book 2))
      2 (count (aggregate-activity "like" :book 3))

      "fuga" (:title (ds/retrieve Book (:book (first (aggregate-activity "like" :book 1)))))
      1 (:point (first (aggregate-activity "like" :book 1)))
      1 (:point (first (aggregate-activity "like" :book 2)))
      2 (:point (first (aggregate-activity "like" :book 3)))
      "hoge" (:title (ds/retrieve Book (:book (first (aggregate-activity "like" :book 3)))))

      1 (count (aggregate-activity "like" :user 1))
      1 (count (aggregate-activity "like" :user 2))
      2 (count (aggregate-activity "like" :user 3))

      "aa" (:name (ds/retrieve User (:user (first (aggregate-activity "like" :user 3)))))
      1 (:point (first (aggregate-activity "like" :user 1)))
      2 (:point (first (aggregate-activity "like" :user 2)))
      2 (:point (first (aggregate-activity "like" :user 3)))
      )
    )
  ) ; }}}

;; == Controller Test ==

(deftest test-controller-list-book
  (create-book "hoge" "" "")
  (ds/save! (assoc (get-book "hoge") :point 2))
  (create-book "fuga" "" "")
  (ds/save! (assoc (get-book "fuga") :point 1))
  (create-book "neko" "" "")

  (are [x y] (= x y)
    3 (count (body->json (testGET "/list/book")))
    1 (count (body->json (testGET "/list/book?limit=1")))
    "hoge" (:title (first (body->json (testGET "/list/book"))))
    "fuga" (:title (second (body->json (testGET "/list/book"))))
    "fuga" (:title (first (body->json (testGET "/list/book?limit=1&page=2"))))
    )
  )

(deftest test-controller-get-user-and-get-book ; {{{
  (let [base "/user/" bbase "/book/"]
    (create-user "aa" "av")
    (create-book "hoge" "hoge-a" "")

    (are [x y z] (= x (-> z body->json y))
      "av" :avatar (testGET base "aa")
      nil :secret-mail (testGET base "aa")
      nil identity (testGET base "unknown")
      "hoge-a" :author (testGET bbase "hoge")
      nil identity (testGET bbase "unknown")
      )
    )
  ) ; }}}

(deftest test-controller-who-like-book ; {{{
  (let [base "/who/like/book?"
        user1 (create-user "aa" "av")
        user2 (create-user "bb" "bv")
        user3 (create-user "cc" "cv")
        book1 (create-book "hoge" "hoge-a" "")
        book2 (create-book "fuga" "fuga-a" "")]

    ; user1 -> book1 = 2 point
    (like-book book1 user1)
    (ds/save! (assoc (get-like-book book1 user1) :point 2))
    ; user1 -> book2 = 1 point
    (like-book book2 user1)
    ; user2 -> book1 = 1 point
    (like-book book1 user2)
    ; user3 -> book2 = 1 point
    (like-book book2 user3)

    (are [x y] (= x y)
      2 (count (body->json (testGET base "user=aa")))
      2 (count (body->json (testGET base "user=aa%2Cbb%2Ccc")))
      1 (count (body->json (testGET base "user=aa%2Cbb%2Ccc&limit=1")))
      3 (:point (first (body->json (testGET base "user=aa%2Cbb%2Ccc"))))
      2 (:point (second (body->json (testGET base "user=aa%2Cbb%2Ccc"))))

      "hoge" (:title (nth-book (body->json (testGET base "user=aa%2Cbb%2Ccc")) 0))
      "fuga" (:title (nth-book (body->json (testGET base "user=aa%2Cbb%2Ccc")) 1))

      2 (count (body->json (testGET base "book=hoge")))
      3 (count (body->json (testGET base "book=hoge%2Cfuga")))
      1 (count (body->json (testGET base "book=hoge%2Cfuga&limit=1")))

      3 (:point (first (body->json (testGET base "book=hoge%2Cfuga"))))
      1 (:point (second (body->json (testGET base "book=hoge%2Cfuga"))))
      "aa" (:name (nth-user :user (body->json (testGET base "book=hoge%2Cfuga")) 0))
      )
    )
  ) ; }}}

(deftest test-controller-who-like-user ; {{{
  (let [base "/who/like/user?"
        user1 (create-user "aa" "")
        user2 (create-user "bb" "")
        user3 (create-user "cc" "")
        user4 (create-user "dd" "")
        fromurl "from_user=aa%2Cbb%2Ccc%2Cdd"
        tourl "to_user=aa%2Cbb%2Ccc%2Cdd"
        ]

    ; user2 -> user1 = 1 point
    (like-user user1 user2)
    (like-user user1 user3)
    (like-user user1 user4)

    (like-user user2 user1)
    (like-user user2 user3)

    (like-user user3 user1)

    (are [x y] (= x y)
      1 (count (body->json (testGET base "from_user=bb")))
      2 (count (body->json (testGET base "from_user=cc")))
      0 (count (body->json (testGET base "from_user=unknown")))
      2 (count (body->json (testGET base "from_user=bb%2Ccc")))
      3 (count (body->json (testGET base fromurl)))
      1 (count (body->json (testGET base fromurl "&limit=1")))
      3 (:point (first (body->json (testGET base fromurl))))
      "aa" (:name (nth-user :to-user (body->json (testGET base fromurl)) 0))
      2 (:point (second (body->json (testGET base fromurl))))
      "bb" (:name (nth-user :to-user (body->json (testGET base fromurl)) 1))
      1 (:point (last (body->json (testGET base fromurl))))
      "cc" (:name (nth-user :to-user (body->json (testGET base fromurl)) 2))

      3 (count (body->json (testGET base "to_user=aa")))
      2 (count (body->json (testGET base "to_user=bb")))
      1 (count (body->json (testGET base "to_user=cc")))
      0 (count (body->json (testGET base "to_user=dd")))
      0 (count (body->json (testGET base "to_user=unknown")))
      4 (count (body->json (testGET base "to_user=aa%2Cbb")))
      4 (count (body->json (testGET base tourl)))

      2 (:point (first (body->json (testGET base tourl))))
      2 (:point (second (body->json (testGET base tourl))))
      2 (:point (second (body->json (testGET base tourl))))
      1 (:point (nth (body->json (testGET base tourl)) 2))
      1 (:point (nth (body->json (testGET base tourl)) 3))
      )
    )
  ) ; }}}

(deftest test-controller-hot-book
  (let [base "/hot/book?"
        user1 (create-user "aa" "") user2 (create-user "bb" "")
        user3 (create-user "cc" "") user4 (create-user "dd" "")
        book1 (create-book "hoge" "" "") book2 (create-book "fuga" "" "")
        book3 (create-book "neko" "" "") ]
    (like-book book1 user1) (update-activity-date book1 user1 (n-days-ago 2))
    (like-book book1 user2) (update-activity-date book1 user2 (n-days-ago 2))
    (like-book book1 user3) (update-activity-date book1 user3 (n-days-ago 2))
    (like-book book2 user2) (update-activity-date book2 user2 (n-days-ago 1))
    (like-book book2 user4) (update-activity-date book2 user4 (n-days-ago 1))
    (like-book book3 user2)

    (are [x y] (= x y)
      3 (count (body->json (testGET base "type=book")))
      1 (count (body->json (testGET base "type=book&day=1")))
      2 (count (body->json (testGET base "type=book&day=2")))
      3 (count (body->json (testGET base "type=book&day=3")))
      0 (count (body->json (testGET base "type=book&day=100")))
      1 (count (body->json (testGET base "type=book&day=3&limit=1")))

      ;"neko" (:title (nth-book (body->json (testGET base "type=book&day=1")) 0))
      "neko" (:title (nth-book (body->json (testGET base "type=book&day=1")) 0))
      "fuga" (:title (nth-book (body->json (testGET base "type=book&day=2")) 0))
      "hoge" (:title (nth-book (body->json (testGET base "type=book&day=3")) 0))
      3 (:point (first (body->json (testGET base "type=book&day=3"))))
      2 (:point (first (body->json (testGET base "type=book&day=3&limit=1&page=2"))))

      4 (count (body->json (testGET base "type=user")))
      1 (count (body->json (testGET base "type=user&day=1")))
      2 (count (body->json (testGET base "type=user&day=2")))
      4 (count (body->json (testGET base "type=user&day=3")))
      0 (count (body->json (testGET base "type=user&day=100")))
      1 (count (body->json (testGET base "type=user&day=3&limit=1")))
      "bb" (:name (nth-user :user (body->json (testGET base "type=user&day=1")) 0))
      1 (:point (first (body->json (testGET base "type=user&day=1"))))
      "bb" (:name (nth-user :user (body->json (testGET base "type=user&day=2")) 0))
      2 (:point (first (body->json (testGET base "type=user&day=2"))))
      "bb" (:name (nth-user :user (body->json (testGET base "type=user&day=3")) 0))
      3 (:point (first (body->json (testGET base "type=user&day=3"))))
      1 (:point (second (body->json (testGET base "type=user&day=3"))))
      1 (:point (nth (body->json (testGET base "type=user&day=3")) 2))
      )
    )
  )

(deftest test-controller-hot-comment ; {{{
  (let [base "/hot/comment?"
        user1 (create-user "aa" "") user2 (create-user "bb" "")
        user3 (create-user "cc" "") user4 (create-user "dd" "")
        book1 (create-book "hoge" "" "") book2 (create-book "fuga" "" "")
        book3 (create-book "neko" "" "") ]
    (create-comment book1 user1 "11") (update-activity-date book1 user1 (n-days-ago 2))
    (create-comment book1 user2 "22") (update-activity-date book1 user2 (n-days-ago 2))
    (create-comment book1 user3 "33") (update-activity-date book1 user3 (n-days-ago 2))
    (create-comment book2 user2 "44") (update-activity-date book2 user2 (n-days-ago 1))
    (create-comment book2 user4 "55") (update-activity-date book2 user4 (n-days-ago 1))
    (create-comment book3 user2 "66")

    (are [x y] (= x y)
      3 (count (body->json (testGET base "type=book")))
      1 (count (body->json (testGET base "type=book&day=1")))
      2 (count (body->json (testGET base "type=book&day=2")))
      3 (count (body->json (testGET base "type=book&day=3")))
      0 (count (body->json (testGET base "type=book&day=100")))
      1 (count (body->json (testGET base "type=book&day=3&limit=1")))

      "neko" (:title (nth-book (body->json (testGET base "type=book&day=1")) 0))
      "fuga" (:title (nth-book (body->json (testGET base "type=book&day=2")) 0))
      "hoge" (:title (nth-book (body->json (testGET base "type=book&day=3")) 0))
      3 (:point (first (body->json (testGET base "type=book&day=3"))))
      2 (:point (first (body->json (testGET base "type=book&day=3&limit=1&page=2"))))

      4 (count (body->json (testGET base "type=user")))
      1 (count (body->json (testGET base "type=user&day=1")))
      2 (count (body->json (testGET base "type=user&day=2")))
      4 (count (body->json (testGET base "type=user&day=3")))
      0 (count (body->json (testGET base "type=user&day=100")))
      1 (count (body->json (testGET base "type=user&day=3&limit=1")))
      "bb" (:name (nth-user :user (body->json (testGET base "type=user&day=1")) 0))
      1 (:point (first (body->json (testGET base "type=user&day=1"))))
      "bb" (:name (nth-user :user (body->json (testGET base "type=user&day=2")) 0))
      2 (:point (first (body->json (testGET base "type=user&day=2"))))
      "bb" (:name (nth-user :user (body->json (testGET base "type=user&day=3")) 0))
      3 (:point (first (body->json (testGET base "type=user&day=3"))))
      1 (:point (second (body->json (testGET base "type=user&day=3"))))
      1 (:point (nth (body->json (testGET base "type=user&day=3")) 2))
      )
    )
  ) ; }}}

(deftest test-controller-search ; {{{
  (let [base "/search?"
        set-point (fn [key point] (ds/save! (assoc (ds/retrieve Book key) :point point)))]
    (create-book "hoge" "hoau" "123") (set-point "hoge" 1)
    (create-book "fuga" "fuau" "456") (set-point "fuga" 2)
    (create-book "neko" "neau" "789") (set-point "neko" 3)
    (create-book "inu" "inau" "101112") (set-point "inu" 4)

    (are [x y] (= x y)
      1 (count (body->json (testGET base "keyword=hoge")))
      2 (count (body->json (testGET base "keyword=g")))
      4 (count (body->json (testGET base "keyword=au")))
      1 (count (body->json (testGET base "keyword=au&limit=1")))
      0 (count (body->json (testGET base "keyword=unknown")))
      1 (count (body->json (testGET base "keyword=123")))
      0 (count (body->json (testGET base "keyword=12")))

      4 (:point (first (body->json (testGET base "keyword=au&limit=1"))))
      "inu" (:title (first (body->json (testGET base "keyword=au&limit=1"))))
      3 (:point (first (body->json (testGET base "keyword=au&limit=1&page=2"))))
      "neko" (:title (first (body->json (testGET base "keyword=au&limit=1&page=2"))))
      )
    )
  ) ; }}}

(deftest test-controller-like-book-history ; {{{
  (let [base "/like/book/history?"
        user1 (create-user "aa" "") user2 (create-user "bb" "")
        book1 (create-book "hoge" "" "") book2 (create-book "fuga" "" "")
        book3 (create-book "neko" "" "")]
    (like-book book1 user1) (update-activity-date book1 user1 (n-days-ago 1))
    (like-book book1 user2) (update-activity-date book1 user2 (n-days-ago 2))
    (like-book book2 user1) (update-activity-date book2 user1 (n-days-ago 3))
    (like-book book3 user1) (update-activity-date book3 user1 (n-days-ago 4))
    (like-book book3 user2) (update-activity-date book3 user2 (n-days-ago 5))

    (are [x y] (= x y)
      3 (count (body->json (testGET base "name=aa")))
      5 (count (body->json (testGET base "name=aa%2Cbb")))
      1 (count (body->json (testGET base "name=aa%2Cbb&limit=1")))

      "aa" (:name (nth-user :user (body->json (testGET base "name=aa%2Cbb")) 0))
      "hoge" (:title (nth-book (body->json (testGET base "name=aa%2Cbb")) 0))
      "hoge" (:title (nth-book (body->json (testGET base "name=bb")) 0))
      "bb" (:name (nth-user :user (body->json (testGET base "name=bb")) 0))
      "fuga" (:title (nth-book (body->json (testGET base "name=aa%2Cbb&limit=1&page=3")) 0))
      )
    )
  ) ; }}}

(deftest test-controller-parts-message ; {{{
  (let [res (testGET "/admin/message/hello")]
    (are [x y] (= x y)
      "hello" (body->json (testGET-with-session res "/parts/message"))
      "" (body->json (testGET-with-session res "/parts/message"))
      )
    )
  ) ; }}}

(deftest test-controller-parts-login ; {{{
  (let [base "/parts/login"
        res (testGET "/admin/login/hoge")
        data (body->json (testGET-with-session res base))]
    (are [x y] (= x y)
      true (loggedin? data)
      "hoge" (login-name data)
      *guest-avatar* (login-avatar data)
      1 (count (get-user-list))
      )

    (let [res2 (testGET-with-session res "/logout")
          data2 (body->json (testGET-with-session res2 base))]
      (are [x y] (= x y)
        false (loggedin? data2)
        nil (login-name data2)
        nil (login-avatar data2)
        )
      )

    ; re login
    (let [res2 (testGET "/admin/login/hoge")
          data2 (body->json (testGET-with-session res2 base))]
      (are [x y] (= x y)
        true (loggedin? data2)
        "hoge" (login-name data2)
        1 (count (get-user-list))
        )
      )
    )
  ) ; }}}

(deftest test-controller-like-book ; {{{
  (create-user "test" "")
  (let [base "/like/book?"
        key-str (entity->key-str (create-book "hoge" "fuga" "12345"))
        key-str2 (entity->key-str (create-book "neko" "inu" "67890"))
        res (testGET "/admin/login/test")
        ;data (body->json (testGET-with-session res base "book=" key-str))
        ;data2 (body->json (testGET-with-session res base "book=" key-str))
        ]

    (testGET-with-session res base "book=" key-str)
    (testGET-with-session res base "book=" key-str)
    (testGET-with-session res base "book=" key-str2 "&point=2")
    (testGET-with-session res base "book=" key-str2 "&point=2")

    (are [x y] (= x y)
      2 (count (get-like-book-list))
      true (today? (:date (first (get-like-book-list))))

      2 (:point (first (get-like-book-list)))
      "neko" (:title (nth-book-entity (get-like-book-list) 0))
      "inu" (:author (nth-book-entity (get-like-book-list) 0))
      "67890" (:isbn (nth-book-entity (get-like-book-list) 0))

      1 (:point (second (get-like-book-list)))
      "hoge" (:title (nth-book-entity (get-like-book-list) 1))
      "fuga" (:author (nth-book-entity (get-like-book-list) 1))
      "12345" (:isbn (nth-book-entity (get-like-book-list) 1))
      )
    )
  ) ; }}}

(deftest test-controller-like-book-new ; {{{
  (let [res (testGET "/admin/login/test")]
    (testPOST {:title "hello"} "/like/book/new")
    (let [book (get-book "hello")]
      (are [x y] (= x y)
        false (nil? book)
        "hello" (:title book)
        nil (:author book)
        nil (:isbn book)
        0 (:point book)
        )
      )

    (testPOST {:title "hoge" :text "fuga"} "/like/book/new")
    (let [activity (first (get-activity-list))]
      (are [x y] (= x y)
        false (nil? activity)
        "comment" (:message activity)
        (ds/get-key-object (get-user *guest-name*)) (:user activity)
        (ds/get-key-object (get-book "hoge")) (:book activity)
        )
      )

    (testPOST-with-session res {:title "hello"} "/like/book/new")
    (is (= 1 (:point (get-book "hello"))))
    (testPOST-with-session res {:isbn "4001156768"} "/like/book/new")
    (let [book (first (find-book :isbn "4001156768" :limit 1))]
      (are [x y] (= x y)
        false (nil? book)
        false (string/blank? (:title book))
        false (string/blank? (:author book))
        false (nil? (:thumbnail book))
        1 (:point book)
        )
      )
    )
  ) ; }}}

(deftest test-controller-post-comment ; {{{
  (create-book "aa" "" "")
  (let [res (testGET "/admin/login/test")
        book-key-str (entity->key-str (get-book "aa"))
        comment1 (body->json (testPOST {:book book-key-str :text "helloworld"} "/post/comment"))
        comment2 (body->json (testPOST-with-session
                               res {:book book-key-str :text "nekonyan"} "/post/comment"))
        guest (get-user *guest-name*)
        ]

    (are [x y] (= x y)
      false (nil? guest)
      ;book-key-str (:book comment1)
      book-key-str (:book_key comment1)
      (entity->key-str guest) (:user_key comment1)
      "helloworld" (:text comment1)
      true (today? (:date comment1))

      ;book-key-str (:book comment2)
      book-key-str (:book_key comment2)
      (entity->key-str (get-user "test")) (:user_key comment2)
      true (today? (:date comment2))
      )
    )
  ) ; }}}

(deftest test-controller-like-book-new-from-mail ; {{{
  (let [from "hoge@fuga.com"
        to (:secret-mail (create-user "aa" ""))
        base-url "/_ah/mail/"]
    (testPOST {:from "hoge@fuga.com" :to *guest-mail* :subject "hoge" :body ""} base-url *guest-mail*)
    (let [book (get-book "hoge")]
      (are [x y] (= x y)
        false (nil? book)
        "hoge" (:title book)
        true (string/blank? (:author book))
        nil (:thumbnail book)
        0 (:point book)
        )
      )

    (testPOST {:from "hoge@fuga.com" :to to :subject "4001156768" :body "hello"} base-url to)
    (let [book (first (find-book :isbn "4001156768" :limit 1))]
      (are [x y] (= x y)
        false (nil? book)
        false (string/blank? (:title book))
        false (string/blank? (:author book))
        "4001156768" (:isbn book)
        false (nil? (:thumbnail book))
        1 (:point book)
        1 (count (get-activity-list))
        )
      )
    )
  ) ; }}}
