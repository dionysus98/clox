(ns clox.env
  (:require [clox.util :refer [undefined-var!]]))

(defn push [env name value]
  (assoc-in env [:env/values name] value))

(defn assign
  [env {lx  :token/lexeme
        :as name} value]
  (if (contains? (:env/values env) lx)
    (push env lx value)
    (undefined-var! name lx)))

(defn pull
  [env {lx  :token/lexeme
        :as name}]
  (let [vs (:env/values env)]
    (if (contains? vs lx)
      (get vs lx)
      (undefined-var! name lx))))

(defn env:new [& {values :values}]
  {:env/values (or values {})})