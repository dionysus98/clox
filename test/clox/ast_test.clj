(ns clox.ast-test
  (:require [clojure.string :as str]
            [clox.ast]))

(defprotocol IAstPrinter
  (print! [this]))

(defmulti print-visitor type)

(defn parenthesize [name & exprs]
  (str
   "(" name " "
   (str/join " " (map #(.accept % print-visitor) exprs))
   ")"))

(defmethod print-visitor
  clox.ast.Binary
  [expr]
  (parenthesize
   (:token/lexeme (.operator expr))
   (.left expr) (.right expr)))

(defmethod print-visitor
  clox.ast.Grouping
  [expr]
  (parenthesize "group" (.expression expr)))

(defmethod print-visitor
  clox.ast.Literal
  [expr]
  (str (.value expr)))

(defmethod print-visitor
  clox.ast.Unary
  [expr]
  (parenthesize
   (:token/lexeme (.operator expr))
   (.right expr)))

(deftype AstPrinter [expr]
  IAstPrinter
  (print! [this] (.accept (.expr this) print-visitor)))
