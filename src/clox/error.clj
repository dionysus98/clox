(ns clox.error)

(defprotocol LoxErrorProtocol
  #_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
  (panic! [err]))

(deftype RuntimeError [token message]
  LoxErrorProtocol
  (panic! [_this] (throw (RuntimeException. message))))