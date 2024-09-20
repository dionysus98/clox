(ns clox.interpreter
  (:require [clox.ast :as ast]))

(defprotocol InterpreterProtocol
  (evaluate [this]))

(defmulti visitor (fn [type _] type))

(defmethod visitor :literal [_ expr]
  (.value expr))

(defmethod visitor :grouping [_ expr]
  (evaluate (.expression expr)))

(defmethod visitor :unary [_ expr]
  (let [right (evaluate (.right expr))
        op    (.operator expr)]
    (case (:token/kind op)
      :BANG  (not (boolean right))
      :MINUS (- (double right))
      nil)))

(defmethod visitor :binary [_ expr]
  (let [left  (evaluate (.left expr))
        right (evaluate (.right expr))
        op    (.operator expr)]
    (case (:token/kind op)
      :GREATER (> left right)
      :GREATER-EQUAL (>= left right)
      :LESS (< left right)
      :LESS-EQUAL (<= left right)
      :BANG-EQUAL (not= left right)
      :EQUAL-EQUAL (= left right)
      :MINUS (- left right)
      :SLASH (/ left right)
      :STAR  (* left right)
      :PLUS  (let [?? #(every? % [left right])]
               (cond
                 (?? number?) (+ left right)
                 (?? string?) (str left right)))
      nil)))

(deftype Interpreter [expr]
  InterpreterProtocol
  (evaluate [this] (ast/accept (.expr this) visitor)))