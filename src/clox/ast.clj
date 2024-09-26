(ns clox.ast
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn read-grammar! [fp]
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

(defn generate-ast!
  "defines protocol and types based on the grammar provided. :c"
  [ast-type fp]
  (when-let [grammar (read-grammar! fp)]
    (eval `(defprotocol ~ast-type
             ~(list 'accept '[this visitor])))
    (doseq [pg   (not-empty grammar)
            :let [type-str (:type pg)
                  sym  (symbol type-str)
                  type (keyword (str/lower-case type-str))]]
      (eval `(deftype ~sym ~(mapv symbol (:fields pg))
               ~ast-type
               ~(list 'accept '[this visitor] (list 'visitor type 'this)))))))

(generate-ast! 'Expr "expression")
(generate-ast! 'Stmt "statement")
