(ns clox.ast-test
  (:require [clojure.string :as str]
            [clox.ast]))

(defprotocol IAstPrinter
  #_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
  (print! [this env]))

(defmulti print-visitor (fn [_ expr] (type expr)))

(defn parenthesize [env name & exprs]
  (str
   "(" name " "
   (str/join " " (map #(.accept % env print-visitor) exprs))
   ")"))

(defmethod print-visitor clox.ast.Binary [env expr]
  (parenthesize
   env
   (:token/lexeme (.operator expr))
   (.left expr) (.right expr)))

(defmethod print-visitor clox.ast.Grouping [env expr]
  (parenthesize env "group" (.expression expr)))

(defmethod print-visitor clox.ast.Literal [_ expr]
  (str (.value expr)))

(defmethod print-visitor clox.ast.Unary [env expr]
  (parenthesize
   env
   (:token/lexeme (.operator expr))
   (.right expr)))

(deftype AstPrinter [expr]
  IAstPrinter
  (print! [this env] (.accept (.expr this) env print-visitor)))
