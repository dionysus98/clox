(ns clox.clox
  (:gen-class)
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clox.scanner :as scr]))

(defn lox:new [src & {fp :file-path}]
  {:lox/had-error? false
   :lox/src-file   fp
   :lox/src        src
   :lox/scanner    nil})

(defn run-src! [lox]
  (let [scr (scr/new (:lox/src lox))
        scr (scr/scan-tokens scr)]
    (doseq [token (:scanner/tokens scr)]
      (println token))
    (assoc lox :lox/scanner scr
           :lox/had-error? (:scanner/had-error? scr))))

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
                   (println "Usage: clox [script]")
                   (System/exit 64))
      (= argc 1) (run-file! (first args))
      :else (run-prompt!))))

(defn -main
  "Crafting Interpreter. CLOX"
  [& args]
  (lox! args))
