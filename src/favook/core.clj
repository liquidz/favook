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
; }}}

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

(defn like-book-controller [{:keys [book point], :or {point "1"}} session]
  (when (loggedin? session)
    (like-book (get-book (str->key book)) (get-user (login-name session))
               :point (parse-int point))
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
  (jsonGET "/parts/login" {session :session} (if (loggedin? session) session {:loggedin false}))

  )

(defroutes main-handler
  (GET "/login" {params :params, session :session}
    (with-message session (redirect "/") "login mada")
    )
  (GET "/logout" _ (with-message (redirect "/") "logged out"))
  )


(defroutes admin-handler
  (GET "/admin/login/:name" {params :params, session :session}
    (login (get params "name") *guest-avatar* session))
  (GET "/admin/message/:text" {params :params, session :session}
    (with-message session (redirect "/admin") (get params "text"))
    )
  )

(defroutes not-found-route
  (not-found "page not found")
  )

(defroutes favook-app-handler
  api-handler
  main-handler
  admin-handler
  not-found-route
  )

(wrap! favook-app-handler session/wrap-session)

(ae/def-appengine-app favook-app #'favook-app-handler)
