(ns clox.util
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [clojure.string :as str]
            [clox.error :refer [->RuntimeError]]))

(defmacro undefined-var! [tk msg]
  `(.panic! (->RuntimeError ~tk (str "undefined variable '" ~msg "'."))))

(defmacro println->
  "like clojure.core/-> but prints the final output before returning."
  [& args]
  `(let [res# (-> ~@args)
         m#   ~(meta &form)]
     (printf (str "----| "
                  ~(name (ns-name *ns*))
                  " | "
                  (:line m#)
                  ":"
                  (:column m#)
                  " >\n\n"
                  (with-out-str (pprint/pprint res#))
                  "\n----end--->\n"))
     res#))

(defmacro println->>
  "like clojure.core/->> but prints the final output before returning."
  [& args]
  `(let [res# (->> ~@args)
         m#   ~(meta &form)]
     (printf (str "----| "
                  ~(name (ns-name *ns*))
                  " | "
                  (:line m#)
                  ":"
                  (:column m#)
                  " >\n\n"
                  (with-out-str (pprint/pprint res#))
                  "\n----end--->\n"))
     res#))

(defn read-grammar!
  "Example Grammar.
   ```
      Literal  : Object value
      Unary    : Token operator, Expr right
      Variable : Token name   
   ```
   each grammar is need to be in it's own line."
  [fp]
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

(defn build-ast
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

(defn generate-ast!
  "defines protocol and types based on the grammar provided. :c
   - `(generate-ast! 'Expr \"expression\")`
   - `(generate-ast! 'Stmt \"statement\")`"
  [ast-type fp]
  (eval (build-ast ast-type fp)))