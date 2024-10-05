(ns clox.main
  (:gen-class)
  #_{:clj-kondo/ignore [:unused-namespace]}
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]
            [clox.util :as util]
            [clox.lexer :as lex]
            [clox.parser :as psr]
            [clox.interpreter :as intr]))

(defn lox:new [src & {fp :file-path}]
  {:lox/had-error?     false
   :lox/runtime-error? false
   :lox/errors         #{}
   :lox/src-file       fp
   :lox/src            src
   :lox/lexer          nil})

(defn run-src! [lox & {env :env}]
  (let [lexer  (->> (:lox/src lox)
                    lex/lexer:new
                    lex/lex)
        parser (->> (:lexer/tokens lexer)
                    psr/parser:new
                    psr/parse)
        intrd  (->> (intr/interpreter:new (:parser/stmts parser) :env env)
                    intr/interpret)
        lox    (-> lox
                   (assoc :lox/lexer lexer
                          :lox/parser parser
                          :lox/interpreter intrd
                          :lox/had-error? (or (:lexer/had-error? lexer)
                                              (:parser/had-error? parser))
                          :lox/runtime-error? (:interpreter/runtime-error? intrd))
                   (update :lox/errors set/union (:lexer/errors lexer) (:parser/errors parser)))]
    ;; (util/println-> intrd)
    lox
    ;; 
    ))

(defn run-prompt! [& {env :env}]
  (print "> ")
  (flush)
  (when-let [line (-> (read-line) str/trim not-empty)]
    (let [lox (run-src! (lox:new line) :env env)]
      (run-prompt! :env (-> lox :lox/interpreter :interpreter/env)))))

(defn run-file! [^String file-path]
  (let [fl (io/file file-path)]
    (assert (.exists fl) (str "filepath doesn't exist " file-path))
    (let [lox (run-src! (lox:new (slurp fl) :file-path file-path))]
      (cond
        (:lox/had-error? lox) (System/exit 65)
        (:lox/runtime-error? lox) (System/exit 70)))))

(defn lox! [args]
  (let [argc (count args)]
    (cond
      (> argc 1) (do
                   (println "Usage: clox [lexipt]")
                   (System/exit 64))
      (= argc 1) (run-file! (first args))
      :else (run-prompt!))))

(defn -main
  "Crafting Interpreter. CLOX"
  [& args]
  (lox! args))
