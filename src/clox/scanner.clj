(ns clox.scanner
  (:require [clox.token :refer [token:new]]
            [clox.util :as util]))

(defn at-end? [scr]
  (>= (:scanner/current scr)
      (count (:scanner/src scr))))

(defn add-token
  ([scr token-type] (add-token scr token-type nil))
  ([{:scanner/keys [start current src line] :as scr} token-type literal]
   (let [text  (subs src start current)]
     (update scr :scanner/tokens conj (token:new token-type text literal line)))))

(defn match? [{:scanner/keys [src current] :as scr}
              expected]
  (cond
    (at-end? scr) false
    (not= (nth src current) expected) false
    :else true))

(defn pk "peek scanner src" [scr]
  (cond
    (at-end? scr) \0
    :else (nth (:scanner/src scr) (:scanner/current scr))))

(defn scan-token [scr]
  (let [cur  (:scanner/current scr)
        src  (:scanner/src scr)
        incc #(update % :scanner/current inc)
        scr  (incc scr)]
    (case (nth src cur)
      \( (add-token scr :left-paren)
      \) (add-token scr :right-paren)
      \{ (add-token scr :left-brace)
      \} (add-token scr :right-brace)
      \, (add-token scr :comma)
      \. (add-token scr :dot)
      \- (add-token scr :minus)
      \+ (add-token scr :plus)
      \; (add-token scr :semicolon)
      \* (add-token scr :star)
      \! (if (match? scr \=)
           (add-token (incc scr) :bang-equal)
           (add-token scr :bang))
      \= (if (match? scr \=)
           (add-token (incc scr) :equal-equal)
           (add-token scr :equal))
      \< (if (match? scr \=)
           (add-token (incc scr) :less-equal)
           (add-token scr :less))
      \> (if (match? scr \=)
           (add-token (incc scr) :greater-equal)
           (add-token scr :greater))
      \/ (if (match? scr \/)
           (loop [scr scr]
             (if (and (not= (pk scr) \newline)
                      (not (at-end? scr)))
               (recur (incc scr))
               scr))
           (add-token scr :slash))
      \newline (update scr :scanner/line inc)
      \space scr
      (do
        (util/report! (:scanner/line scr) "Unexpected character.")
        (assoc scr :scanner/had-error? true)))))

(defn scan-tokens [scanner]
  (let [run (fn scan-next [{:scanner/keys [current line]
                            :as           scr}]
              (if-not (at-end? scr)
                (-> scr
                    (assoc :scanner/start current)
                    scan-token
                    scan-next)
                (update scr :scanner/tokens conj (token:new :eof "" nil line))))]
    (run scanner)))

(defn new [^String src]
  {:scanner/src        src
   :scanner/tokens     []
   :scanner/line       1
   :scanner/start      0
   :scanner/current    0
   :scanner/had-error? false})

(comment
  (scan-tokens (clox.scanner/new "var * = (+ 5 8); // asasas
                                  asas"))

  (loop [scr (clox.scanner/new (slurp "examples/main.clox"))]
    (println (pr-str (pk scr)))
    (when-not (at-end? scr)
      (recur (update scr :scanner/current inc))))
  :rcf)

