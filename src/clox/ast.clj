(ns clox.ast
  "very Boilerplate-ish :c")

(defprotocol Ast
  "Clox Ast Visitor protocol"
  #_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
  (accept [this visitor] [this env visitor]))

;; EXPRESSION
(deftype Assign [name value]
  Ast
  (accept [this visitor] (visitor :assign this))
  (accept [this env visitor] (visitor :assign env this)))

(deftype Binary [left operator right]
  Ast
  (accept [this visitor] (visitor :binary this))
  (accept [this env visitor] (visitor :binary env this)))

(deftype Call [callee paren arguments]
  Ast
  (accept [this visitor] (visitor :call this))
  (accept [this env visitor] (visitor :call env this)))

(deftype Grouping [expression]
  Ast
  (accept [this visitor] (visitor :grouping this))
  (accept [this env visitor] (visitor :grouping env this)))

(deftype Literal [value]
  Ast
  (accept [this visitor] (visitor :literal this))
  (accept [this env visitor] (visitor :literal env this)))

(deftype Logical [left operator right]
  Ast
  (accept [this visitor] (visitor :logical this))
  (accept [this env visitor] (visitor :logical env this)))

(deftype Unary [operator right]
  Ast
  (accept [this visitor] (visitor :unary this))
  (accept [this env visitor] (visitor :unary env this)))

(deftype Variable [name]
  Ast
  (accept [this visitor] (visitor :variable this))
  (accept [this env visitor] (visitor :variable env this)))

;; STATEMENTS
(deftype Block [statements]
  Ast
  (accept [this visitor] (visitor :block this))
  (accept [this env visitor] (visitor :block env this)))

(deftype Expression [expression]
  Ast
  (accept [this visitor] (visitor :expression this))
  (accept [this env visitor] (visitor :expression env this)))

(deftype Function [name params body]
  Ast
  (accept [this visitor] (visitor :function this))
  (accept [this env visitor] (visitor :function env this)))

(deftype If [condition then-branch else-branch]
  Ast
  (accept [this visitor] (visitor :if this))
  (accept [this env visitor] (visitor :if env this)))

(deftype Print [expression]
  Ast
  (accept [this visitor] (visitor :print this))
  (accept [this env visitor] (visitor :print env this)))

(deftype Return [keyword value]
  Ast
  (accept [this visitor] (visitor :return this))
  (accept [this env visitor] (visitor :return env this)))

(deftype Var [name initializer]
  Ast
  (accept [this visitor] (visitor :var this))
  (accept [this env visitor] (visitor :var env this)))

(deftype While [condition body]
  Ast
  (accept [this visitor] (visitor :while this))
  (accept [this env visitor] (visitor :while env this)))
