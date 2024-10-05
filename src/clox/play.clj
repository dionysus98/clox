2 (ns clox.play
    (:require [clox.interpreter :as intr]
              [clox.lexer :as lex]
              [clox.parser :as psr]))

(let [tks   (->> "print (7 + 8) * (7 * (8 + 9));"
                 lex/lexer:new
                 lex/lex
                 :lexer/tokens)
      stmts (->> tks
                 psr/parser:new
                 psr/parse
                 :parser/stmts)
      intr (intr/interpreter:new stmts)]
  (intr/interpret intr))
