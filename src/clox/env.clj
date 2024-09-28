(ns clox.env
  (:require [clox.error :refer [->RuntimeError]]))

(defn push [env name value]
  (assoc-in env [:env/values name] value))

(defn pull
  [env {lx  :token/lexeme
        :as name}]
  (let [vs (:env/values env)]
    (if (contains? vs lx)
      (get vs lx)
      (.panic! (->RuntimeError name (str "undefined variable '" lx "' ."))))))

(defn env:new [& {values :values}]
  {:env/values (or values {})})