(ns ds-util
  (:require
     [appengine-magic.services.datastore :as ds]
     )
  (:import
     [com.google.appengine.api.datastore Key KeyFactory ]
     )
  )

(defn key? [obj] (instance? Key obj))
(defn key->str [obj] (if (key? obj) (KeyFactory/keyToString obj)))
(defn str->key [obj] (if (string? obj) (KeyFactory/stringToKey obj)))
;(defn entity? [obj] (instance? (:on-interface ds/EntityProtocol) obj))
(defn entity? [obj] (extends? ds/EntityProtocol (class obj)))
