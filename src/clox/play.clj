 (ns clox.play
  (:require [clox.interpreter :as intr]
            [clox.lexer :as lex]
            [clox.parser :as psr]))

(let [src   "var a = (7 + 8) * (7 * (8 + 9)); 
             print a;"
      tks   (->> src
                 lex/lexer:new
                 lex/lex
                 :lexer/tokens)
      stmts (->> tks
                 psr/parser:new
                 psr/parse
                 :parser/stmts)
      intr  (intr/interpreter:new stmts)]
  (intr/interpret intr))

(let [src   "for (var a = 10; a > 0; a = a - 2) { print a; }"
      tks   (->> src
                 lex/lexer:new
                 lex/lex
                 :lexer/tokens)
      stmts (->> tks
                 psr/parser:new
                 psr/parse
                 :parser/stmts)
      intr  (intr/interpreter:new stmts)]
  (intr/interpret intr))