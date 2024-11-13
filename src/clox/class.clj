(ns clox.class
  (:refer-clojure :exclude [class get set])
  (:require
   [clox.callable :refer [ILoxCallable]]
   [clox.error :refer [->RuntimeError]]))

(defprotocol ILoxInstance
  (get [this name-token])
  (set [this name-token value]))

(defprotocol ILoxClass
  (find-method [this name]))

(deftype LoxInstance [class fields]
  Object
  (toString [_] (str (.name class) " instance"))

  ILoxInstance
  (get [_ {lx  :token/lexeme
           :as name-token}]
    (if (contains? fields lx)
      (get fields lx)
      (if-some [method (.find-method class lx)]
        method
        (->RuntimeError name-token (str "undefined property " lx)))))

  (set [this {lx :token/lexeme} value]
    (->LoxInstance (.class this) (assoc (.fields this) lx value))))

(deftype LoxClass [name methods]
  ILoxClass
  (find-method [_  name]
    (when (contains? methods name)
      (get methods name)))
  ILoxCallable
  (arity [_] 0)
  (call  [this]
    {:expr (->LoxInstance this {})})
  (call  [this _ _]
    (.call this))
  Object
  (toString [_] (str name)))
