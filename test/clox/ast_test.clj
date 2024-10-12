(ns clox.ast-test
  (:require [clojure.string :as str]))

(defprotocol IAstPrinter
  #_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
  (print! [this env]))

(defmulti print-visitor (fn [type _ _] type))

(defn parenthesize [env name & exprs]
  (str
   "(" name " "
   (str/join " " (map #(.accept % env print-visitor) exprs))
   ")"))

(defmethod print-visitor :binary [_ env expr]
  (parenthesize
   env
   (:token/lexeme (.operator expr))
   (.left expr) (.right expr)))

(defmethod print-visitor :grouping [_ env expr]
  (parenthesize env "group" (.expression expr)))

(defmethod print-visitor :literal [_ _ expr]
  (str (.value expr)))

(defmethod print-visitor :unary [_ env expr]
  (parenthesize
   env
   (:token/lexeme (.operator expr))
   (.right expr)))

(deftype AstPrinter [expr]
  IAstPrinter
  (print! [this env] (.accept (.expr this) env print-visitor)))
