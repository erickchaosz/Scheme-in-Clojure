(ns env.core
  (:gen-class))



(defn add-binding-to-frame!
  [var val frame])


(defn make-frame
  "Make a new frame which is a map with vars as keys and vals as values"
  [vars vals]
  (into {} (map vector vars vals)))

(defn extend-env
  [vars vals base-env])


(defn lookup-variable-value
  [var env])

(defn set-variable-value!
  [var val env])


(defn define-variable!
  [var val env]
  )
