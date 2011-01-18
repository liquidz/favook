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
(defn- testGET [url] (favook-app-handler (request :get url)))

;; Entity Definitions {{{
(ds/defentity User [^:key name avatar secret-mail date])
(ds/defentity Book [^:key title author isbn])
(ds/defentity LikeBook [book user point date])
(ds/defentity LikeUser [to-user from-user point date])
(ds/defentity Comment [book user text date])
(ds/defentity Activity [book user message date])
; }}}

;; Util Test
(deftest test-entity? ; {{{
  (is (entity? (User. "a" "b" "c" "d")))
  ) ; }}}

;; Model Test
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

(deftest test-get-like-book-list
  (let [user1 (create-user "aa" "") user2 (create-user "bb" "") user3 (create-user "cc" "")
        book1 (create-book "hoge" "" "") book2 (create-book "fuga" "" "") book3 (create-book "neko" "" "")]
    (like-book book1 user1) (like-book book2 user1) (like-book book3 user1)
    (ds/save! (assoc (first (ds/query :kind LikeBook :filter [(= :user user1) (= :book book3)])) :point 3))
    (ds/save! (assoc (first (ds/query :kind LikeBook :filter [(= :user user1) (= :book book2)])) :point 2))
    (like-book book1 user2) (like-book book3 user2)

    (are [x y] (= x y)
      3 (count (get-like-book-list user1))
      2 (count (get-like-book-list user2))
      0 (count (get-like-book-list user3))
      1 (count (get-like-book-list user1 :limit 1))
      "hoge" (:title (ds/retrieve Book (:book (first (get-like-book-list user1 :limit 1)))))
      "fuga" (:title (ds/retrieve Book (:book (first (get-like-book-list user1 :limit 1 :page 2)))))
      )
    )
  )

(deftest test-like-user ; {{{
  (let [user1 (create-user "aa" "")
        user2 (create-user "bb" "")
        ]
    (ds/save! [user1 user2])

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

(deftest test-controller
  (create-user "masa" "")

  (is (= "hello" (:body (testGET "/echo/hello"))))

  ; json response
  (are [x y z] (= x (-> y :body json/read-json z))
    "masa" (testGET "/user/masa") :name
    (today) (testGET "/user/masa") :date
    nil (testGET "/user/masa") :secret-mail
    nil (testGET "/user/donotexistuser") identity
    )
  )

