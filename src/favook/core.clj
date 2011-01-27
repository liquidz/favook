(ns favook.core
  (:use
     [compojure.core :only [GET POST defroutes wrap!]]
     [compojure.route :only [not-found]]
     [ring.util.response :only [redirect]]
     [favook constants util model]
     ds-util
     )
  (:require
     [appengine-magic.core :as ae]
     [appengine-magic.services.datastore :as ds]
     [ring.middleware.session :as session]
     [clojure.contrib.string :as string]

     [appengine-magic.services.url-fetch :as uf]
     )
  )

(defmacro json-service [method path bind & body] ; {{{
  `(~method ~path ~bind
      (let [res# (do ~@body)]
        (if (and (map? res#) (contains? res# :status) (contains? res# :headers) (contains? res# :body))
          (assoc res# :body (to-json (:body res#)))
          (to-json res#)))))
(defmacro jsonGET [path bind & body] `(json-service GET ~path ~bind ~@body))
(defmacro jsonPOST [path bind & body] `(json-service POST ~path ~bind ~@body))
(defmacro apiGET [path fn] `(jsonGET ~path {params# :params} (~fn (convert-map params#))))
(defmacro apiGET-with-session [path fn] `(jsonGET ~path {params# :params, session# :session}
                                                  (~fn (convert-map params#) session#)))
(defmacro apiPOST [path fn] `(jsonPOST ~path {params# :params} (~fn (convert-map params#))))
(defmacro apiPOST-with-session [path fn] `(jsonPOST ~path {params# :params, session# :session}
                                                  (~fn (convert-map params#) session#)))
; }}}

(defn init-guest-user []
  (if (nil? (get-user *guest-name*))
    (create-user *guest-name* *guest-avatar* :guest? true)
    )
  )

(defn login [name avatar session]
  (let [user (aif (get-user name) it (create-user name avatar))]
    (with-session
      session
      (redirect "/")
      {
       :name (:name user)
       :avatar (:avatar user)
       :loggedin true
       }
      )
    )
  )

; == Controllers ==
(defn point-like-book-controller [params] ; {{{
  (let [[limit page] (params->limit-and-page params)
        key (if (:user params) :user (if (:book params) :book))
        data (if key (remove nil? (map (if (= key :user) get-user get-book)
                                       (string/split #"\s*,\s*" (key params)))))
        likedata (if-not (empty? data) (flatten (map #(get-like-book-list key %) data)))
        ]

    (case key
      :user (take limit (aggregate-entity-points :book likedata))
      :book (take limit (aggregate-entity-points :user likedata))
      ()
      )
    )
  ) ; }}}

(defn point-like-user-controller [params] ; {{{
  (let [[limit page] (params->limit-and-page params)
        [key val] (aif (:to_user params) [:to-user it] (aif (:from_user params) [:from-user it]))
        data (if key (remove nil? (map get-user (string/split #"\s*,\s*" val))))
        likedata (if-not (empty? data) (flatten (map #(get-like-user-list key %) data)))
        ]
    (case key
      :to-user
      (take limit (aggregate-entity-points :from-user likedata))
      :from-user
      (take limit (aggregate-entity-points :to-user likedata))
      ()
      )
    )
  ) ; }}}

(defn point-comment-controller [params] ; {{{
  (let [[limit page] (params->limit-and-page params)
        type (aif (:type params) (if (= it "user") :user :book) :book)
        day (aif (:day params) (parse-int it) 7)
        ]
    (when (and (pos? day) (<= day 7))
      (take limit
            (drop (* limit (dec page)) (aggregate-activity "comment" type day)))
      )
    )
  ) ; }}}

(defn search-controller [params] ; {{{
  (let [[limit page] (params->limit-and-page params)
        books (get-book-list :all? true)
        keyword (:keyword params)
        num (* limit page)
        ]
    (when-not (string/blank? keyword)
      (take limit (drop (* limit (dec page)) (take-if
        num
        #(or
           (not= -1 (.indexOf (:title %) keyword))
           (not= -1 (.indexOf (:author %) keyword))
           (= (:isbn %) keyword)
           ) books)))
      )
    )
  ) ; }}}

(defn- get-activity-history [message params] ; {{{
  (let [[limit page] (params->limit-and-page params)
        users (remove nil? (map get-user (string/split #"\s*,\s*" (:name params))))
        res (flatten (map #(get-activity-list :user % :message message :limit (* limit page)) users))
        ]
    (take limit (drop (* limit (dec page)) (sort #(str-comp >= (:date %1) (:date %2)) res)))
    )
  ) ; }}}

(defn like-book-history-controller [params] ; {{{
  (let [[limit page] (params->limit-and-page params)
        users (remove nil? (map get-user (string/split #"\s*,\s*" (:name params))))
        res (flatten (map #(get-activity-list :user % :message "like" :limit (* limit page)) users))
        ]
    (take limit (drop (* limit (dec page)) (sort #(str-comp >= (:date %1) (:date %2)) res)))
    )
  )
(def like-book-history-controller (partial get-activity-history "like"))
(def history-comment-controller (partial get-activity-list "comment")) ; }}}

(defn like-book-new-controller [{:keys [title author isbn]
                                 :or {title nil, author nil, isbn nil}
                                 :as params} session #^User user]
  (let [book (create-book title author isbn :fill? true)]
    (if (nil? book)
      (with-message session (redirect "/") "posted data is incorrect")
      (when (loggedin? session)
        (like-book book user)
        (redirect "/")
        )
      )
    )
  )

(defn like-book-new-from-mail-controller [{:keys [subject from to body]}]
  (let [user (get-user-from-mail to)]
    (when-not (nil? user)
      (let [isbn-flag (isbn? subject)
            title (if-not isbn-flag subject)
            isbn (if isbn-flag subject)
            book (create-book title nil isbn :fill? true)
            ]
        (if (nil? book)
          "" ; (send-error-mail from "posted data is incorrect")
          (like-book book user)
          )
        )
      )
    )
  )

(defn like-book-controller [{:keys [book point], :or {point "1"}} session]
  (when (loggedin? session)
    (like-book (get-book (str->key book)) (get-user (login-name session))
               :point (parse-int point))
    )
  )

(defn post-comment-controller [{:keys [book text] :as params} session]
  (init-guest-user)
  (let [book-entity (get-book (str->key book))
        user (if (loggedin? session) (get-user (:name session)) (get-user *guest-name*))
        ]
    (create-comment book-entity user text)
    )
  )


(defroutes api-handler
  (apiGET "/user/:name" get-user)
  (apiGET "/book/:title" get-book)
  ; like/book/point
  (apiGET "/point/like/book" point-like-book-controller)
  ; like/user/point
  (apiGET "/point/like/user" point-like-user-controller)
  ; comment/point
  (apiGET "/point/comment" point-comment-controller)
  (apiGET "/search" search-controller)
  ; like/book/history
  (apiGET "/like/book/history" like-book-history-controller)
  ; comment/history
  (apiGET "/history/comment" history-comment-controller)

  (apiGET-with-session "/like/book" like-book-controller)

  (jsonGET "/parts/message" {session :session} (with-message session (:message session) ""))
  (jsonGET "/parts/login" {session :session} (if (loggedin? session)
                                               (dissoc session :user)
                                               {:loggedin false}))

  (apiPOST-with-session "/post/comment" post-comment-controller)

  )

(defroutes main-handler
  (GET "/login" {params :params, session :session}
    (with-message session (redirect "/") "login mada")
    )
  (GET "/logout" _ (with-message (redirect "/") "logged out"))

  (POST "/like/book/new" {params :params, session :session}
    (like-book-new-controller
      (convert-map params)
      session
      (if (loggedin? session) (get-user (:name session)) (get-user *guest-name*))
      )
    )

  (POST "/_ah/mail/*" {params :params} (like-book-new-from-mail-controller (convert-map params)) "")

  )


(defroutes admin-handler
  (GET "/admin/login/:name" {params :params, session :session}
    (login (get params "name") *guest-avatar* session))
  (GET "/admin/message/:text" {params :params, session :session}
    (with-message session (redirect "/admin") (get params "text"))
    )
  )

(defroutes not-found-route
  (GET "/test" req
    (println req)
    "test route"
    )

  (not-found "page not found")
  )


(defroutes favook-app-handler
  api-handler
  main-handler
  admin-handler
  not-found-route
  )

;(init-guest-user)
(wrap! favook-app-handler session/wrap-session)
(ae/def-appengine-app favook-app #'favook-app-handler)
