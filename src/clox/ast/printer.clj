(ns clox.ast.printer
  (:require [clox.token :as t]
            [clox.ast.main :as ast]
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


(comment
  #_{:clj-kondo/ignore [:unresolved-var]}
  (= (print!
      (AstPrinter.
       (let [left
             (ast/->Unary
              (t/token:new :MINUS "-" nil 1)
              (ast/->Literal 123))
             op    (t/token:new :STAR "*" nil 1)
             right (ast/->Grouping (ast/->Literal 45.67))]
         (ast/->Binary left op right))))
     "(* (- 123) (group 45.67))")
  :rcf)

