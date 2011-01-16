(ns favook.core
  (:use
     [compojure.core :only [GET defroutes]]
     [compojure.route :only [not-found]]
     [clojure.contrib.json :only [json-str]]
     )
  (:require
     [appengine-magic.core :as ae]
     [appengine-magic.services.datastore :as ds]

     [clojure.contrib.string :as string]
     )
  )


;(defn- json-conv [obj]
;  (cond
;    (or (seq? obj) (list? obj)) (map json-conv obj)
;    ;(map? obj) (map-val-map json-conv (remove-extra-key obj))
;    (key? obj) (key->str obj)
;    :else obj
;    )
;  )
;(defn to-json [obj] (json-str (json-conv obj)))


(def delete-html-tag (partial string/replace-re #"<.+?>" ""))

(defn convert-map [m]
  (apply
    hash-map
    (interleave
      (map keyword (keys m))
      (map (comp string/trim delete-html-tag) (vals m))))
  )


(defmacro json-service [method path bind & body] ; {{{
  `(~method ~path ~bind
      (let [res# (do ~@body)]
        (if (and (map? res#) (contains? res# :status) (contains? res# :headers) (contains? res# :body))
          (assoc res# :body (json-str (:body res#)))
          (json-str res#)))))
(defmacro jsonGET [path bind & body] `(json-service GET ~path ~bind ~@body))
;(defmacro jsonPOST [path bind & body] `(json-service POST ~path ~bind ~@body))
(defmacro apiGET [path fn] `(jsonGET ~path {params# :params} (~fn (convert-map params#))))
;(defmacro apiGET/session [path fn] `(jsonGET ~path {params# :params, session# :session}
;                                             (~fn (convert-map params#) session#)))
; }}}

(defroutes favook-app-handler
  (GET "/hoge" _
    {:status 200
     :headers {"Content-Type" "text/plain"}
     :body "hogehoge"}
    )
  (not-found "page not found")
  )


(ae/def-appengine-app favook-app #'favook-app-handler)
