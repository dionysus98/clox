(ns clox.env
  (:require [clox.error :refer [->RuntimeError]]))

(defn -def [env name value]
  (assoc-in env [:env/values name] value))

(defn -get [{vs :env/values} {lx :token/lexeme :as name}]
  (if (contains? vs lx)
    (get vs lx)
    (.panic! (->RuntimeError name (str "undefined variable '" lx "' .")))))

(defn env:new []
  {:env/values {}})