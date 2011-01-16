(ns favook.app_servlet
  (:gen-class :extends javax.servlet.http.HttpServlet)
  (:use favook.core)
  (:use [appengine-magic.servlet :only [make-servlet-service-method]]))


(defn -service [this request response]
  ((make-servlet-service-method favook-app) this request response))
