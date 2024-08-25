(ns clox.util
  (:require [clojure.java.io :as io]))

(defn lazy-file-lines
  "## `(lazy-file-lines \"/tmp/massive-file.txt\")`"
  [file]
  (letfn [(helper [rdr]
            (lazy-seq
             (if-let [line (.readLine rdr)]
               (cons line (helper rdr))
               (do (.close rdr) nil))))]
    (helper (io/reader file))))

(defn report!
  ([^Number line ^String msg]
   (report! line "" msg))
  ([^Number line ^String where ^String msg]
   (println (str "[line " line "] Error" where ": " msg))))
