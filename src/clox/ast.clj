(ns clox.ast
  "very Boilerplate-ish :c")

(defprotocol Ast
  "Clox Ast Visitor protocol"
  #_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
  (accept [this visitor] [this env visitor]))

  ;; EXPRESSION
(deftype Assign [name value]
  Ast
  (accept [this visitor] (visitor this))
  (accept [this env visitor] (visitor env this)))

(deftype Binary [left operator right]
  Ast
  (accept [this visitor] (visitor this))
  (accept [this env visitor] (visitor env this)))

(deftype Call [callee paren arguments]
  Ast
  (accept [this visitor] (visitor this))
  (accept [this env visitor] (visitor env this)))

(deftype Get [object name]
  Ast
  (accept [this visitor] (visitor this))
  (accept [this env visitor] (visitor env this)))

(deftype Grouping [expression]
  Ast
  (accept [this visitor] (visitor this))
  (accept [this env visitor] (visitor env this)))

(deftype Literal [value]
  Ast
  (accept [this visitor] (visitor this))
  (accept [this env visitor] (visitor env this)))

(deftype Logical [left operator right]
  Ast
  (accept [this visitor] (visitor this))
  (accept [this env visitor] (visitor env this)))

(deftype Unary [operator right]
  Ast
  (accept [this visitor] (visitor this))
  (accept [this env visitor] (visitor env this)))

(deftype Variable [name]
  Ast
  (accept [this visitor] (visitor this))
  (accept [this env visitor] (visitor env this)))

  ;; STATEMENTS
(deftype Block [statements]
  Ast
  (accept [this visitor] (visitor this))
  (accept [this env visitor] (visitor env this)))

(deftype LoxClass [name methods]
  Ast
  (accept [this visitor] (visitor this))
  (accept [this env visitor] (visitor env this)))

(deftype Expression [expression]
  Ast
  (accept [this visitor] (visitor this))
  (accept [this env visitor] (visitor env this)))

(deftype Function [name params body]
  Ast
  (accept [this visitor] (visitor this))
  (accept [this env visitor] (visitor env this)))

(deftype If [condition then-branch else-branch]
  Ast
  (accept [this visitor] (visitor this))
  (accept [this env visitor] (visitor env this)))

(deftype Print [expression]
  Ast
  (accept [this visitor] (visitor this))
  (accept [this env visitor] (visitor env this)))

(deftype Return [keyword value]
  Ast
  (accept [this visitor] (visitor this))
  (accept [this env visitor] (visitor env this)))

(deftype Var [name initializer]
  Ast
  (accept [this visitor] (visitor this))
  (accept [this env visitor] (visitor env this)))

(deftype While [condition body]
  Ast
  (accept [this visitor] (visitor this))
  (accept [this env visitor] (visitor env this)))
  