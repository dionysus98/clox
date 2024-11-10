(ns clox.class
  (:refer-clojure :exclude [class])
  (:require
   [clox.callable :refer [ILoxCallable]]))

(deftype LoxInstance [class]
  Object
  (toString [_] (str (.name class) " instance")))

(deftype LoxClass [name]
  ILoxCallable
  (arity [_] 0)
  (call  [this intr args]
    {:expr (->LoxInstance this)})
  Object
  (toString [_] (str name)))

