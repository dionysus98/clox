(ns clox.callable)

(defprotocol ILoxCallable
  (arity [this])
  (env   [this] [this interpreter])
  (call  [this] [this interpreter] [this interpreter arguments]))

(deftype Clock []
  ILoxCallable
  (arity [_] 0)
  (call  [_]
    (/ (System/currentTimeMillis) 1000.0))
  (call  [_ _ _]
    (/ (System/currentTimeMillis) 1000.0))
  Object
  (toString [_] "<native fn>"))

(comment
  (.call (Clock.))
  :rcf)


