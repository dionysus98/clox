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

(def token-type
  (set/union single-char-tokens multi-char-tokens literals keywords #{:eof}))

(defn token:new
  ([type]
   (case type
     :eof (token:new type "" nil 0)
     (token:new type "" nil 0)))
  ([type lexeme literal line]
   (assert (token-type type) (str "Type " type " should be a valid TokenType"))
   {:token/type    type
    :token/lexeme  lexeme
    :token/literal literal
    :token/line    line}))