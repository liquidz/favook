(defproject favook "1.0.0-SNAPSHOT"
  :description "FIXME: write"
  :dependencies [[org.clojure/clojure "1.2.0"]
                 [org.clojure/clojure-contrib "1.2.0"]
                 ;[org.clojars.liquidz/compojure "0.5.3"]
                 [compojure "0.5.3"]
;                 [ring/ring-core "0.3.5"]
  ;               [appengine-magic "0.3.2"]
                 ]
  :dev-dependencies [[appengine-magic "0.3.2"]
                     [ring-mock "0.1.1"]
                     ]
  :aot [favook.app_servlet]
  )
