(ns clox.scanner
  (:require [clox.token :refer [token:new]]))

(defn add-token
  ([scr token-type] (add-token scr token-type nil))
  ([{:scanner/keys [start current src line] :as scr} token-type literal]
   (let [text  (subs src start current)]
     (update scr :scanner/tokens conj (token:new token-type text literal line)))))

(defn recognize-token [scr]
  (let [cur (:scanner/current scr)
        src (:scanner/src scr)]
    (case (nth src cur)
      \( :left-paren
      \) :right-paren
      \{ :left-brace
      \} :right-brace
      \, :comma
      \. :dot
      \- :minus
      \+ :plus
      \; :semicolon
      \* :star
      nil)))

(defn scan-tokens [scanner]
  (let [src  (:scanner/src scanner)
        srcc (count src)
        scan (fn scan-token [{:scanner/keys [current line]
                              :as           scr}]
               (if-not (>= current srcc)
                 (let [scr   (assoc scr :scanner/start current)
                       token (recognize-token scr)
                       scr   (update scr :scanner/current inc)
                       scr   (if token (add-token scr token) scr)]
                   (scan-token scr))
                 (update scr :scanner/tokens conj (token:new :eof "" nil line))))]
    (scan scanner)))

(defn scanner:new [^String src]
  {:scanner/src     src
   :scanner/tokens  []
   :scanner/line    1
   :scanner/start   0
   :scanner/current 0})

;; (scan-tokens (scanner:new "var * = (+ 5 8);"))
