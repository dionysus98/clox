(ns clox.ast-test
  (:require [clox.ast :as ast]
            [clojure.string :as str]))

(defprotocol AstPrinterProtocol
  (print! [printer]))

(defmulti print-visitor (fn [type _] type))

(defn parenthesize [name & exprs]
  (str
   "(" name " "
   (str/join " " (map #(ast/accept % print-visitor) exprs))
   ")"))

(defmethod print-visitor :binary [_ expr]
  (parenthesize
   (:token/lexeme (.operator expr))
   (.left expr) (.right expr)))

(defmethod print-visitor :grouping [_ expr]
  (parenthesize "group" (.expression expr)))

(defmethod print-visitor :literal [_ expr]
  (str (.value expr)))

(defmethod print-visitor :unary [_ expr]
  (parenthesize
   (:token/lexeme (.operator expr))
   (.right expr)))

(deftype AstPrinter [expr]
  AstPrinterProtocol
  (print! [this] (ast/accept (.expr this) print-visitor)))
