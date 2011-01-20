(ns favook.test.core
  (:use
     [favook core model util]
     ds-util
     :reload)
  (:use clojure.test ring.mock.request)

  (:require
     [appengine-magic.testing :as ae-testing]
     [appengine-magic.services.datastore :as ds]
     [clojure.contrib.json :as json]
     )
  )

(use-fixtures :each (ae-testing/local-services :all))

(defn- foreach [f l] (doseq [x l] (f x)))
(defn- body->json [res] (-> res :body json/read-json))
(defn- testGET [url] (favook-app-handler (request :get url)))

;; Entity Definitions {{{
(ds/defentity User [^:key name avatar secret-mail point date])
(ds/defentity Book [^:key title author isbn point])
(ds/defentity LikeBook [book user point date])
(ds/defentity LikeUser [to-user from-user point date])
(ds/defentity Comment [book user text date])
(ds/defentity Activity [book user message date])
; }}}

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

(deftest test-get-book ; {{{
  (create-book "title" "" "")
  (is (= "title" (:title (get-book "title"))))
  (is (= "title" (:title (get-book {:title "title"}))))
  (is (nil? (get-book "unknown")))
  ) ; }}}

(deftest test-get-book-list ; {{{
  (let [user1 (create-user "aa" "") user2 (create-user "bb" "") user3 (create-user "cc" "")
        book1 (create-book "hoge" "" "") book2 (create-book "fuga" "" "") book3 (create-book "neko" "" "")]
    (like-book book1 user1) (like-book book1 user2) (like-book book1 user3)
    (like-book book2 user1) (like-book book2 user2)

    (are [x y] (= x y)
      3 (count (get-book-list))
      2 (count (get-book-list :limit 2))
      "hoge" (:title (first (get-book-list)))
      3 (:point (first (get-book-list)))
      "fuga" (:title (first (get-book-list :limit 1 :page 2)))
      2 (:point (first (get-book-list :limit 1 :page 2)))
      "neko" (:title (last (get-book-list)))
      0 (:point (last (get-book-list)))
      )
    )
  ) ; }}}

(deftest test-find-book ; {{{
  (create-book "hello" "aa" "") (ds/save! (assoc (ds/retrieve Book "hello") :point 3))
  (create-book "neko" "bb" "") (ds/save! (assoc (ds/retrieve Book "neko") :point 2))
  (create-book "inu" "bb" "") (ds/save! (assoc (ds/retrieve Book "inu") :point 1))
  (create-book "wani" "cc" "")

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
    )
  ) ; }}}

(deftest test-like-book ; {{{
  (let [user1 (create-user "aa" "")
        user2 (create-user "bb" "")
        book (create-book "title" "" "")]
    (ds/save! [user1 user2 book])

    (like-book book user1)
    (is (= 1 (ds/query :kind LikeBook :count-only? true)))
    (like-book book user2)
    (is (= 2 (ds/query :kind LikeBook :count-only? true)))
    (like-book book user1)
    (is (= 2 (ds/query :kind LikeBook :count-only? true)))
    (let [like (first (ds/query :kind LikeBook :filter (= :user user1)))]
      (is (= 1 (:point like)))
      (ds/save! (assoc like :date "1900-01-01"))
      (like-book book user1)
      (is (= 2 (:point (first (ds/query :kind LikeBook :filter (= :user user1))))))
      )
    )
  ) ; }}}

(deftest test-get-like-book-list ; {{{
  (let [user1 (create-user "aa" "") user2 (create-user "bb" "") user3 (create-user "cc" "")
        book1 (create-book "hoge" "" "") book2 (create-book "fuga" "" "") book3 (create-book "neko" "" "")]
    (like-book book1 user1) (like-book book2 user1) (like-book book3 user1)
    (ds/save! (assoc (first (ds/query :kind LikeBook :filter [(= :user user1) (= :book book3)])) :point 3))
    (ds/save! (assoc (first (ds/query :kind LikeBook :filter [(= :user user1) (= :book book2)])) :point 2))
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

(deftest test-get-like-user-list ; {{{
  (let [user1 (create-user "aa" "")
        user2 (create-user "bb" "")
        user3 (create-user "cc" "")
        ]
    (like-user user2 user1)
    (ds/save! (assoc (first (ds/query :kind LikeUser :filter [(= :to-user user2) (= :from-user user1)])) :point 2))
    (like-user user3 user1)
    (like-user user3 user2)

    (are [x y] (= x y)
      3 (count (get-like-user-list))
      2 (count (get-like-user-list :limit 2))

      2 (count (get-like-user-list :from-user user1))
      1 (count (get-like-user-list :from-user user1 :limit 1))
      2 (:point (first (get-like-user-list :from-user user1)))
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
    (ds/save! (assoc (first (ds/query :kind Activity :filter [(= :user user) (= :book book1)])) :date "1900-01-01"))

    (are [x y] (= x y)
      2 (count (get-activity-list :user user))
      1 (count (get-activity-list :user user :limit 1))
      1 (count (get-activity-list :book book1))

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
        book1 (create-book "hoge" "" "") book2 (create-book "fuga" "" "")
        update-date
        (fn [b u d] (ds/save! (assoc (first (ds/query :kind Activity :filter [(= :book b) (= :user u)])) :date d)))]
    (like-book book1 user1) (update-date book1 user1 (n-days-ago 1))
    (like-book book2 user1)
    (like-book book1 user2) (update-date book1 user2 (n-days-ago 2))

    (are [x y] (= x y)
      1 (count (aggregate-activity "like" 1))
      2 (count (aggregate-activity "like" 2))
      2 (count (aggregate-activity "like" 3))

      "fuga" (:title (ds/retrieve Book (:book (first (aggregate-activity "like" 1)))))
      1 (:point (first (aggregate-activity "like" 1)))
      1 (:point (first (aggregate-activity "like" 2)))
      2 (:point (first (aggregate-activity "like" 3)))
      "hoge" (:title (ds/retrieve Book (:book (first (aggregate-activity "like" 3)))))
      )
    )
  ) ; }}}

;; == Controller Test ==
(deftest test-controller-get-user-and-get-book ; {{{
  (create-user "aa" "av")
  (create-book "hoge" "hoge-a" "")
  (are [x y z] (= x (-> z body->json y))
    "av" :avatar (testGET "/user/aa")
    nil :secret-mail (testGET "/user/aa")
    nil identity (testGET "/user/unknown")
    "hoge-a" :author (testGET "/book/hoge")
    )
  ) ; }}}

(deftest test-controller-like-book ; {{{
  (let [user1 (create-user "aa" "av")
        user2 (create-user "bb" "bv")
        user3 (create-user "cc" "cv")
        book1 (create-book "hoge" "hoge-a" "")
        book2 (create-book "fuga" "fuga-a" "")]

    ; user1 -> book1 = 2 point
    (like-book book1 user1)
    (ds/save! (assoc (first (ds/query :kind LikeBook :filter [(= :book book1) (= :user user1)])) :point 2))
    ; user1 -> book2 = 1 point
    (like-book book2 user1)
    ; user2 -> book1 = 1 point
    (like-book book1 user2)
    ; user3 -> book2 = 1 point
    (like-book book2 user3)

    (are [x y] (= x y)
      2 (count (body->json (testGET "/like/book?user=aa")))
      2 (count (body->json (testGET "/like/book?user=aa%2Cbb%2Ccc")))
      1 (count (body->json (testGET "/like/book?user=aa%2Cbb%2Ccc&limit=1")))
      3 (:point (first (body->json (testGET "/like/book?user=aa%2Cbb%2Ccc"))))
      2 (:point (second (body->json (testGET "/like/book?user=aa%2Cbb%2Ccc"))))

      "hoge" (:title (ds/retrieve Book (str->key (:book (first (body->json (testGET "/like/book?user=aa%2Cbb%2Ccc")))))))
      "fuga" (:title (ds/retrieve Book (str->key (:book (second (body->json (testGET "/like/book?user=aa%2Cbb%2Ccc")))))))

      2 (count (body->json (testGET "/like/book?book=hoge")))
      3 (count (body->json (testGET "/like/book?book=hoge%2Cfuga")))
      1 (count (body->json (testGET "/like/book?book=hoge%2Cfuga&limit=1")))

      3 (:point (first (body->json (testGET "/like/book?book=hoge%2Cfuga"))))
      1 (:point (second (body->json (testGET "/like/book?book=hoge%2Cfuga"))))
      "aa" (:name (ds/retrieve User (str->key (:user (first (body->json (testGET "/like/book?book=hoge%2Cfuga")))))))
      )
    )
  ) ; }}}

(deftest test-controller-like-user ; {{{
  (let [user1 (create-user "aa" "")
        user2 (create-user "bb" "")
        user3 (create-user "cc" "")
        user4 (create-user "dd" "")
        ]

    ; user2 -> user1 = 1 point
    (user-like user1 user2)
    (user-like user1 user3)
    (user-like user1 user4)


    ;(testGET "/like/user?")
    )
  ) ; }}}
