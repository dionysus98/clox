(ns clox.class
  (:refer-clojure :exclude [class])
  (:require
   [clox.callable :refer [ILoxCallable]]
   [clox.error :refer [->RuntimeError]]))

(defprotocol ILoxInstance
  (pull [this name-token])
  (push [this name-token value]))

(defprotocol ILoxClass
  (find-method [this name]))

(deftype LoxInstance [class fields]
  Object
  (toString [_] (str (.name class) " instance"))

  ILoxInstance
  (pull [this {lx  :token/lexeme
               :as name-token}]
    (if (contains? fields lx)
      (get fields lx)
      (if-some [method (.find-method class lx)]
        (.bind method this)
        (.panic! (->RuntimeError name-token (str "undefined property " lx))))))

  (push [this {lx :token/lexeme} value]
    (LoxInstance. (.class this) (assoc (.fields this) lx value))))

(deftype LoxClass [name methods]
  ILoxClass
  (find-method [_  name]
    (when (contains? methods name)
      (get methods name)))
  ILoxCallable
  (arity [_] 0)
  (call  [this]
    {:expr   (LoxInstance. this {})})
  (call  [this _ _]
    (.call this))
  Object
  (toString [_] (str name)))
