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

(defn- foreach [f l] (doseq [x l] (f x)))

(ds/defentity TestEntity [^:key a b])
(ds/defentity SubEntity [^:key c d])

(deftest test-entity?
  (is (entity? (TestEntity. "a" "b")))
  )


;(ds/defentity User [^:key name login-type secret-mail])
(ds/defentity User [^:key name secret-mail point date])
(ds/defentity Book [^:key title author isbn])
(ds/defentity Favorite [book user point date])
(ds/defentity Comment [book user text date])
(ds/defentity Activity [book user message date])

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

(deftest test-user
  (let [u1 (User. "Alice" "")
        u2 (User. "Bob" "")
        b1 (Book. "aaa" "bbb" "")
        b2 (Book. "ccc" "ddd" "")
        b3 (Book. "eee" "fff" "")
        ]
    (ds/save! [u1 u2 b1 b2 b3
               (Favorite. b1 u1 2 (Date.))
               (Favorite. b1 u2 1 (Date.))
               (Favorite. b2 u1 1 (Date.))
               (Favorite. b3 u2 1 (Date.))
               (Comment. b1 u1 "zzz" (Date.))
               (Comment. b2 u1 "yyy" (Date.))
               (Comment. b1 u2 "xxx" (Date.))
               (Comment. b3 u2 "www" (Date.))
               ])

    (ds/query :kind Favorite )
    )
  )

;(deftest test-hoge
;  (let [alice (User. 1 "Alice" "twitter" "")
;        bob (User. 2 "Bob" "guest" "")
;        masa (User. 3 "masa" "facebook" "")
;        book (Book. "title" "isbn" "neko")
;        book2 (Book. "hoge" "fuga" "inu")
;        book3 (Book. "uo" "chan" "aaa")
;        fav (Favorite. book alice 2 (Date.))
;        fav2 (Favorite. book bob 1 (Date.))
;        fav3 (Favorite. book2 alice 1 (Date.))
;        fav4 (Favorite. book3 masa 1 (Date.))
;        ]
;    (ds/save! [alice bob book book2 book3 fav fav2 fav3 fav4])
;
;    ;(doseq [x (ds/query :kind Favorite :filter (= :user alice) :sort [:point])]
;    ;  (println x)
;    ;  )
;    )
;  )
