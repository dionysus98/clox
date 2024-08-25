(ns clox.clox
  (:gen-class)
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn report! [^Number line ^String where ^String msg]
  (println "[line "  line "] Error" where ": " msg))

(defn scanner [^String src]
  (str/split src #" "))

(defn run-src! [^String src]
  (let [tokens (scanner src)]
    (doseq [token tokens]
      (println token))))

(defn run-prompt []
  (print "> ")
  (flush)
  (when-let [line (-> (read-line) str/trim not-empty)]
    (run-src! line) (run-prompt)))

(defn run-file [^String file-path]
  (let [fl (io/file file-path)]
    (assert (.exists fl) (str "filepath doesn't exist " file-path))
    (run-src! (slurp fl))))

(defn lox [& args]
  (let [argc (count args)]
    (cond
      (> argc 1) (do
                   (println "Usage: clox [script]")
                   (System/exit 64))
      (= argc 1) (run-file (first args))
      :else (run-prompt))))

(defn -main
  "Crafting Interpreter. CLOX"
  [& args]
  (apply lox args))
