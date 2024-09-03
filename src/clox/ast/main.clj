(ns clox.ast.main
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defprotocol Expr
  #_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
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

(doseq [pg   (not-empty parsed-grammar)
        :let [sym (symbol (:key pg))
              type (keyword (str/lower-case (:key pg)))]]
  (eval `(deftype ~sym ~(mapv symbol (:fields pg))
           Expr
           ~(list 'accept '[this visitor] (list 'visitor type 'this)))))

#_(when-let [data (not-empty parsed-grammar)]
    (eval `(defprotocol Visitor
             ~@(for [pg   data
                     :let [proto (symbol (str "visit" (:name pg)))]]
                 (list proto '[expr])))))

#_(doseq [pg   (not-empty parsed-grammar)
          :let [visitor (symbol (str "visit" (:name pg)))]]
    (eval `(defrecord ~(symbol (:name pg)) ~(mapv symbol (:fields pg))
             Expr
             ~(list 'accept ['this] (list visitor 'this))
             Visitor
             ~(list visitor ['this] 'this)))) 