(ns clox.ast
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defprotocol Expr
  (accept [this visitor]))

(defonce ast
  (when-let [f (io/resource "ast")]
    (let [lines (line-seq (io/reader f))
          clean (comp str/lower-case str/trim)]
      (for [line lines
            :let [[k v] (map str/trim (str/split line #":"))
                           ;; ignoring the type hint from grammar for now
                  field (comp last #(str/split % #" ") clean)
                  fields (mapv field (str/split v #","))]]
        {:type   k
         :fields fields}))))

; :c defines types based on the grammar provided. :c
(doseq [pg   (not-empty ast)
        :let [sym (symbol (:type pg))
              type (keyword (str/lower-case (:type pg)))]]
  (eval `(deftype ~sym ~(mapv symbol (:fields pg))
           Expr
           ~(list 'accept '[this visitor] (list 'visitor type 'this)))))
