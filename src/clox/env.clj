(ns clox.env
  (:require [clox.error :refer [->RuntimeError]]))

;; SUBJECT TO CHANGE.
;; the entire state of the language is maintained in an atom as a hashmap, not ideal.

(def !env "LOX state"
  (atom {:env/values {}}))

(defn push [name value]
  (swap! !env assoc-in [:env/values name] value))

(defn pull
  [{lx  :token/lexeme
    :as name}]
  (let [vs (:env/values @!env)]
    (if (contains? vs lx)
      (get vs lx)
      (.panic! (->RuntimeError name (str "undefined variable '" lx "' ."))))))

(defn env:new "Returns state-atom !env after it resets !env to default state" []
  (reset! !env {:env/values {}})
  !env)