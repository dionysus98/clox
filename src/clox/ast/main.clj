(ns clox.ast.main
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defprotocol Expr
  (accept [this visitor]))

(def grammar
  (when-let [f (io/resource "grammar")]
    (line-seq (io/reader f))))

(def parsed-grammar
  (let [base  "Expr"
        clean (comp str/lower-case str/trim)]
    (vec (for [line grammar
               :let [[k v] (map str/trim (str/split line #":"))
                   ;; ignoring the type from grammar hint for now
                     field (comp last #(str/split % #" ") clean)
                     fields (mapv field (str/split v #","))
                     rec    (str k base)]]
           {:base   base
            :name   rec
            :key    k
            :fields fields}))))

; :c defines types based on the grammer provided. :c
(doseq [pg   (not-empty parsed-grammar)
        :let [sym (symbol (:key pg))
              type (keyword (str/lower-case (:key pg)))]]
  (eval `(deftype ~sym ~(mapv symbol (:fields pg))
           Expr
           ~(list 'accept '[this visitor] (list 'visitor type 'this)))))
