(ns clox.main
  (:gen-class)
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [clojure.set :as set]
            [clojure.string :as str]
            [clox.lexer :as lex]
            [clox.parser :as psr]))

(defn lox:new [src & {fp :file-path}]
  {:lox/had-error? false
   :lox/errors     #{}
   :lox/src-file   fp
   :lox/src        src
   :lox/lexer      nil})

(defn run-src! [lox]
  (let [lexer  (->> (:lox/src lox)
                    lex/lexer:new
                    lex/lex)
        lexed  (-> lox
                   (assoc :lox/lexer lexer
                          :lox/had-error? (:lexer/had-error? lexer))
                   (update :lox/errors set/union (:lexer/errors lexer)))
        parser (->> (:lexer/tokens lexer)
                    psr/parser:new
                    psr/parse)
        parsed (-> lexed
                   (assoc :lox/parser parser
                          :lox/had-error? (:parser/had-error? parser))
                   (update :lox/errors set/union (:parser/errors parser)))]
    (pprint/pprint parsed)
    lox))

(defn run-prompt! []
  (print "> ")
  (flush)
  (when-let [line (-> (read-line) str/trim not-empty)]
    (run-src! (lox:new line)) (run-prompt!)))

(defn run-file! [^String file-path]
  (let [fl (io/file file-path)]
    (assert (.exists fl) (str "filepath doesn't exist " file-path))
    (run-src! (lox:new (slurp fl) :file-path file-path))))

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
