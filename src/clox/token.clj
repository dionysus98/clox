(ns clox.token
  (:require [clojure.set :as set]))

(deftype Token [kind lexeme literal line])

(def single-char-tokens
  #{:LEFT-PAREN
    :RIGHT-PAREN
    :LEFT-BRACE
    :RIGHT-BRACE
    :COMMA
    :DOT
    :MINUS
    :PLUS
    :SEMICOLON
    :SLASH
    :STAR})

(def multi-char-tokens
  #{:BANG, :BANG-EQUAL
    :EQUAL, :EQUAL-EQUAL
    :GREATER, :GREATER-EQUAL
    :LESS, :LESS-EQUAL})

(def literals
  #{:IDENT :STRING :NUMBER})

(def keywords
  #{:AND :CLASS :ELSE :FALSE :FUN :FOR :IF :NIL :OR
    :PRINT :RETURN :SUPER :THIS :TRUE :VAR :WHILE})

(def token-kind
  (set/union single-char-tokens multi-char-tokens literals keywords #{:EOF}))

(defn token:new
  ([kind]
   (case kind
     :EOF (token:new kind "" nil 0)
     (token:new kind "" nil 0)))
  ([kind lexeme literal line]
   (assert (token-kind kind) (str "kind " kind " should be a valid Tokenkind"))
   {:token/kind    kind
    :token/lexeme  lexeme
    :token/literal literal
    :token/line    line}))