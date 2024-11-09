(ns clox.error)

(defprotocol ILoxError
  #_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
  (panic! [err]))

(deftype RuntimeError [token message]
  ILoxError
  (panic! [_this] (throw (RuntimeException. message))))

(deftype ParserError [psr message]
  ILoxError
  (panic! [_this] (throw (ex-info (str message) psr))))

(deftype ResolverError [resl message]
  ILoxError
  (panic! [_this] (throw (ex-info message resl))))