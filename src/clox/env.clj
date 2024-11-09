(ns clox.env
  (:require [clox.util :refer [undefined-var!]]))

(defn push [env name value]
  (assoc-in env [:env/values name] value))

(defn assign
  [{encl :env/enclosing
    :as  env}
   {lx  :token/lexeme
    :as tk}
   value]
  (let [vs (:env/values env)]
    (cond
      (contains? vs lx) (push env lx value)
      encl  (update env :env/enclosing assign tk value)
      :else (undefined-var! tk lx))))

(defn pull
  [{encl :env/enclosing
    :as  env}
   {lx  :token/lexeme
    :as tk}
   & {safe? :safe?}]
  (let [vs (:env/values env)]
    (cond
      (contains? vs lx) (get vs lx)
      encl (pull encl tk :safe? safe?)
      :else (when-not safe? (undefined-var! tk lx)))))

(defn ancestor [env ^Integer distance]
  (let [reach    (repeat distance :env/enclosing)
        enclosed (get-in env reach)]
    enclosed))

(defn pull-at [env ^Integer distance ^String name]
  (-> (ancestor env distance)
      :env/values
      (get name)))

(defn assign-at [env ^Integer distance tk value]
  (let [reach    (repeat (dec distance) :env/enclosing)]
    (update-in env reach assoc (:token/lexeme tk) value)))

(defn env:new
  [& {values :env/values
      encl   :env/enclosing}]
  {;; enclosing is a also to an env obj
   :env/enclosing encl
   :env/values    (or values {})})