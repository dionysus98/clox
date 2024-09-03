(ns clox.main
  (:gen-class)
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clox.lexer :as lex]))

(defn lox:new [src & {fp :file-path}]
  {:lox/had-error? false
   :lox/src-file   fp
   :lox/src        src
   :lox/lexer      nil})

(defn run-src! [lox]
  (let [lex (lex/lexer:new (:lox/src lox))
        lex (lex/scan-tokens lex)
        lox (assoc lox
                   :lox/lexer lex
                   :lox/had-error? (:lexer/had-error? lex))]
    (doseq [token (:lexer/tokens lex)]
      (println token))
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
