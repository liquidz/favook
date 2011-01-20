(ns favook.core
  (:use
     [compojure.core :only [GET POST defroutes]]
     [compojure.route :only [not-found]]
     [favook util model]
     ds-util
     )
  (:require
     [appengine-magic.core :as ae]
     [appengine-magic.services.datastore :as ds]

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
;(defmacro jsonPOST [path bind & body] `(json-service POST ~path ~bind ~@body))
(defmacro apiGET [path fn] `(jsonGET ~path {params# :params} (~fn (convert-map params#))))
;(defmacro apiGET/session [path fn] `(jsonGET ~path {params# :params, session# :session}
;                                             (~fn (convert-map params#) session#)))
; }}}

(defn aggregate-entity-points [group-keyword entity-list]
  (sort
    #(> (:point %1) (:point %2))
    (map #(reduce (fn [res x] (assoc res :point (+ (:point res) (:point x)))) %)
         (vals (group-by group-keyword entity-list))))
  )
(defn- params->limit-and-page [params]
  [(aif (:limit params) (parse-int it) *default-limit*)
   (aif (:page p) (parse-int it) 1)])

(defroutes json-handler
  (apiGET "/user/:name" get-user)
  (apiGET "/book/:title" get-book)
  (jsonGET "/like/book" {params :params}
           (let [p (convert-map params)
                 [limit page] (params->limit-and-page p)

                 key (if (:user p) :user (if (:book p) :book))
                 data (if key (map (if (= key :user) get-user get-book) (string/split #"\s*,\s*" (key p))))
                 likedata (flatten (map #(get-like-book-list key %) data))
                 ]

             (case key
               :user (take limit (aggregate-entity-points :book likedata))
               :book (take limit (aggregate-entity-points :user likedata))
               ()
               )
             )
           )
  (jsonGET "/like/user" {params :params}
           (let [p (convert-map params)
                 [limit page] (params->limit-and-page p)
                 ;key (if (:to_user p) :to_user (if (:from_user p) :from_user))

                 [key val] (aif (:to_user p) [:to-user it] (aif (:from_user p) [:from-user it]))

                 data (if key (map get-user (string/split #"\s*,\s*" val)))
                 likedata (flatten (map #(get-like-user-list key %) data))
                 ]
             (case key
               :to-user
               (take limit (aggregate-activity :from-user likedata))
               :from-user
               (take limit (aggregate-activity :to-user likedata))
               ()
               )
             )
           )
  )

(defroutes main-handler
  (GET "/hoge" _
    {:status 200
     :headers {"Content-Type" "text/plain"}
     :body "hogehoge"}
    )
  (POST "/post" {params :params, session :session}
    "post"
    )
  )

(defroutes test-handler

  (GET "/echo/:text" {params :params}
    (get params "text")
    )
  )

(defroutes not-found-route
  (not-found "page not found")
  )

(defroutes favook-app-handler
  json-handler
  main-handler
  test-handler
  not-found-route
  )

;(wrap! app session/wrap-session)


(ae/def-appengine-app favook-app #'favook-app-handler)
