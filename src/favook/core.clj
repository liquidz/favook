(ns favook.core
  (:use
     [compojure.core :only [GET POST defroutes]]
     [compojure.route :only [not-found]]
     [clojure.contrib.json :only [json-str]]
     [favook util model]
     ds-util
     )
  (:require
     [appengine-magic.core :as ae]
     [appengine-magic.services.datastore :as ds]

     [clojure.contrib.string :as string]
     )
  )

(defn remove-extra-key [m]
  (dissoc m :secret-mail)
  )

(defn- json-conv [obj]
  (cond
    (or (seq? obj) (list? obj)) (map json-conv obj)
    (map? obj) (map-val-map json-conv (remove-extra-key obj))
    (key? obj) (key->str obj)
    :else obj
    )
  )
(defn to-json [obj] (json-str (json-conv obj)))


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

(defroutes json-handler
  (apiGET "/user/:name" get-user)
  (apiGET "/book/:title" get-book)
  ;(jsonGET "/like/book" {params :params}
  ;         (let [p (convert-map params)
  ;               limit (-> p :limit parse-int)
  ;               page (-> p :page parse-int)
  ;               users (aif (:user p) (map get-user (string/split #"\s*,\s*" it)))
  ;               books (aif (:book p) (map get-book (string/split #"\s*,\s*" it)))
  ;               ]
  ;           (cond
  ;             users
  ;             (sort
  ;               #(> (:point %1) (:point %2))
  ;               (map
  ;               (fn [l]
  ;                 (reduce
  ;                   (fn [res x]
  ;                     (assoc res :point (+ (:point res) (:point x)))
  ;                     )
  ;                   l
  ;                   )
  ;                 )
  ;               (vals (group-by :book (map #(get-like-book-list :user % :limit limit) users)))))
  ;             :else ()
  ;             )
  ;           )
  ;         )
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
