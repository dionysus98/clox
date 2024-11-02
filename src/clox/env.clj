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

(defn env:new
  [& {values :env/values
      encl   :env/enclosing}]
  {;; enclosing is a also to an env obj
   :env/enclosing encl
   :env/values    (or values {})})