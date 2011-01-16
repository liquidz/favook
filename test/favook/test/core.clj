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
  )

(use-fixtures :each (ae-testing/local-services :all))

(ds/defentity TestEntity [^:key a b])

(ds/defentity Book [^:key title, author])
(ds/defentity Comment [^:key text])

(defn print-entities [obj]
  (if (or (seq? obj) (list? obj))
    (doseq [e obj] (print-entities e))
    (println (str "[" (type obj) "]: " obj))
    )
  )

(deftest test-entity?
  (is (entity? (TestEntity. "a" "b")))
  )

;(deftest test-hoge
;  (let [b1 (Book. "hello" "world")
;        b2 (Book. "neko" "nyan")
;        [k1 k2] (ds/save! [b1 b2])
;        ]
;    (ds/save! (ds/new* Comment ["hogecomment"] :parent k1))
;    (ds/save! (ds/new* Comment ["nekocomment"] :parent k2))
;
;    (println (type (ds/query)))
;    )
;  )
