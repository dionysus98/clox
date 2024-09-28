(ns clox.error)

(defprotocol LoxErrorProtocol
  #_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
  (panic! [err]))

;; using records over types just for the easier interface.
(defrecord RuntimeError [token message]
  LoxErrorProtocol
  (panic! [_this] (throw (RuntimeException. message))))