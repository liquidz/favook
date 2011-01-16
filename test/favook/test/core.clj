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

(ds/defentity TestEntity [^:key a b] :kind "testentity")
(ds/defentity SubEntity [^:key c d] :kind "subentity")

(defn print-entities [obj]
  (if (or (seq? obj) (list? obj))
    (doseq [e obj] (print-entities e))
    (println (str "[" (type obj) "]: " obj))
    )
  )

(deftest test-entity?
  (is (entity? (TestEntity. "a" "b")))
  )

(deftest test-hoge
  (let [[t s] (ds/save! [(TestEntity. 1 2) (SubEntity. 3 4)])]
    (println (ds/retrieve TestEntity t))
    (println (ds/retrieve SubEntity s))

    (println "====")
    (println (ds/retrieve SubEntity t))
    (println (ds/retrieve TestEntity s))
    (println "====")
    (println (ds/retrieve SubEntity t :kind "subentity"))
    (println (ds/retrieve TestEntity s :kind "testentity"))
    )
  )
