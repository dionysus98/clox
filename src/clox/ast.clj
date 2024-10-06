(ns clox.ast
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn- read-grammar! [fp]
  (when-let [f (io/resource fp)]
    (let [lines (line-seq (io/reader f))
          clean (comp str/lower-case str/trim)]
      (for [line lines
            :let [[k v] (map str/trim (str/split line #":"))
                             ;; ignoring the type hint from grammar for now
                  field (comp last #(str/split % #" ") clean)
                  fields (mapv field (str/split v #","))]]
        {:type   k
         :fields fields}))))

#_{:clj-kondo/ignore [:unused-private-var]}
(defn- build-ast
  "builds protocol and types based on the grammar provided. :c
   - `(build-ast 'Expr \"expression\")`
   - `(build-ast 'Stmt \"statement\")`"
  [ast-type fp]
  (when-let [grammar (read-grammar! fp)]
    (let [proto `(defprotocol ~ast-type
                   ~(list 'accept '[this env visitor]))
          types (for [pg   (not-empty grammar)
                      :let [type-str (:type pg)
                            sym  (symbol type-str)
                            type (keyword (str/lower-case type-str))]]
                  `(deftype ~sym ~(mapv symbol (:fields pg))
                     ~ast-type
                     ~(list 'accept '[this env visitor] (list 'visitor type 'env 'this))))]
      (->> types
           (cons proto)
           (cons 'do)))))

#_{:clj-kondo/ignore [:unused-private-var]}
(defn- generate-ast!
  "defines protocol and types based on the grammar provided. :c
   - `(generate-ast! 'Expr \"expression\")`
   - `(generate-ast! 'Stmt \"statement\")`"
  [ast-type fp]
  (eval (build-ast ast-type fp)))

(defprotocol Visitor
  #_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
  (accept [this env visitor]))

;; EXPRESSION
(deftype Assign [name value]
  Visitor
  (accept [this env visitor] (visitor :assign env this)))

(deftype Binary [left operator right]
  Visitor
  (accept [this env visitor] (visitor :binary env this)))

(deftype Grouping [expression]
  Visitor
  (accept [this env visitor] (visitor :grouping env this)))

(deftype Literal [value]
  Visitor
  (accept [this env visitor] (visitor :literal env this)))

(deftype Unary [operator right]
  Visitor
  (accept [this env visitor] (visitor :unary env this)))

(deftype Variable [name]
  Visitor
  (accept [this env visitor] (visitor :variable env this)))

;; STATEMENTS
(deftype Block [statements]
  Visitor
  (accept [this env visitor] (visitor :block env this)))

(deftype Expression [expression]
  Visitor
  (accept [this env visitor] (visitor :expression env this)))

(deftype Print [expression]
  Visitor
  (accept [this env visitor] (visitor :print env this)))

(deftype Var [name initializer]
  Visitor
  (accept [this env visitor] (visitor :var env this)))