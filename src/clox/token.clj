(ns clox.token
  (:require [clojure.set :as set]))

(def single-char-tokens
  #{:left-paren
    :right-paren
    :left-brace
    :right-brace
    :comma
    :dot
    :minus
    :plus
    :semicolon
    :slash
    :star})

(def multi-char-tokens
  #{:bang, :bang-equal
    :equal, :equal-equal
    :greater, :greater-equal
    :less, :less-equal})

(def literals
  #{:ident :string :number})

(def keywords
  #{:and :class :else :false :fun :for :if :nil :or
    :print :return :super :this :true :var :while})

(def token-kind
  (set/union single-char-tokens multi-char-tokens literals keywords #{:eof}))

(defn token:new
  ([kind]
   (case kind
     :eof (token:new kind "" nil 0)
     (token:new kind "" nil 0)))
  ([kind lexeme literal line]
   (assert (token-kind kind) (str "kind " kind " should be a valid Tokenkind"))
   {:token/kind    kind
    :token/lexeme  lexeme
    :token/literal literal
    :token/line    line}))