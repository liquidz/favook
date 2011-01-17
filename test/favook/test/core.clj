(ns favook.test.core
  (:use
     [favook.core]
     ds-util
     :reload)
  (:use [clojure.test])

  (:require
     [appengine-magic.testing :as ae-testing]
     [appengine-magic.services.datastore :as ds]
     )
  (:import java.util.Date)
  )

(use-fixtures :each (ae-testing/local-services :all))

(ds/defentity TestEntity [^:key a b])
(ds/defentity SubEntity [^:key c d])

(defn print-entities [obj]
  (if (or (seq? obj) (list? obj))
    (doseq [e obj] (print-entities e))
    (println (str "[" (type obj) "]: " obj))
    )
  )

(deftest test-entity?
  (is (entity? (TestEntity. "a" "b")))
  )


(ds/defentity User [^:key uid name login-type secret-mail])
(ds/defentity Book [^:key title isbn author])
(ds/defentity Favorite [book user point date])
(ds/defentity Comment [book user text date])

; {{{
;(defn bytes->hex-str [bytes]
;  (apply str (map #(string/tail 2 (str "0" (Integer/toHexString (bit-and 0xff %)))) bytes))
;  )
;(defn digest-hex [algorithm s]
;  (if-not (string/blank? s)
;    (-> (MessageDigest/getInstance algorithm) (.digest (.getBytes s)) bytes->hex-str)
;    )
;  )
;(def str->md5 (partial digest-hex "MD5"))
;(def str->sha1 (partial digest-hex "SHA1"))
; }}}

(deftest test-hoge
  (let [alice (User. 1 "Alice" "twitter" "")
        bob (User. 2 "Bob" "guest" "")
        masa (User. 3 "masa" "facebook" "")
        book (Book. "title" "isbn" "neko")
        book2 (Book. "hoge" "fuga" "inu")
        book3 (Book. "uo" "chan" "aaa")
        fav (Favorite. book alice 2 (Date.))
        fav2 (Favorite. book bob 1 (Date.))
        fav3 (Favorite. book2 alice 1 (Date.))
        fav4 (Favorite. book3 masa 1 (Date.))
        ]
    (ds/save! [alice bob book book2 book3 fav fav2 fav3 fav4])

    ;(println (ds/query :kind Favorite :filter (= :user alice)))

    ;(doseq [x (conj (ds/query :kind Favorite :filter (= :user alice))
    ;                (ds/query :kind Favorite :filter (= :user bob)))]
    ;  (println x)
    ;  )
    (doseq [x (ds/query :kind Favorite :filter (= :user alice) :sort [:point])]
      (println x)
      )
    )
  )
