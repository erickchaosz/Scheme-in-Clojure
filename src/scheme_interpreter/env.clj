(ns scheme-interpreter.env
  (:gen-class))

(def empty-env (list))
(defn enclosing-env [env] (rest env))
(defn first-frame [env] (first env))

(defn add-binding-to-frame!
  [var val frame]
  (swap! frame assoc var val))


(defn make-frame
  "Make a new frame which is an atomic map with vars as keys and vals as values"
  [vars vals]
  (let [frame (atom {})]
    (do (swap! frame
               (fn [frame]
                 (into frame (map
                              vector
                              vars vals))))
        frame)))

(defn extend-env
  "Extend the environment by creating a new frame and attaching it to the base-env"
  [vars vals base-env]
  (if (= (count vars) (count vals))
    (let [new-frame (make-frame vars vals)]
      (conj base-env new-frame))
    (throw (Exception. "Number of variables and values should be equal"))))

(defn- env-loop
  "Do found does things if found else do-nil"
  [var val env do-found do-nil exception]
   (if (empty? env)
     (exception var)
    (let [current-frame (first-frame env)
          val? (get @current-frame var)
          rest-env (enclosing-env env)]
      (if val?
        (do-found env)
        (do-nil env)
        ))))

(defn lookup-variable-value
  [var env]
  (let [do-found (fn [new-env]
                   (let [current-frame (first-frame new-env)]
                     (get @current-frame var)))
        do-nil (fn [new-env]
                 (lookup-variable-value var (enclosing-env new-env)))
        exception (fn [var]
                    (throw (Exception. (str "Unbound variable " var))))]
    (env-loop var nil env do-found do-nil exception)))

(defn set-variable-value!
  [var val env]
  (let [do-found (fn [new-env]
                    (add-binding-to-frame! var val(first-frame new-env)))
         do-nil (fn [new-env]
                  (set-variable-value! var val (enclosing-env new-env)))
         exception (fn [var]
                     (throw (Exception. (str "Unbound variable" var))))]
     (env-loop var val env do-found do-nil exception)))

(defn define-variable!
  [var val env]
  (let [do-found (fn [new-env]
                   (add-binding-to-frame! var val(first-frame new-env)))
        do-nil do-found
        exception (fn [var]
                    (throw (Exception. "No Frame")))]
    (env-loop var val env do-found do-nil exception)))
