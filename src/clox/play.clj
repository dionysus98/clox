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

(let [src   "event(name, args, 5);"
      tks   (->> src
                 lex/lexer:new
                 lex/lex
                 :lexer/tokens)
      stmts (->> tks
                 psr/parser:new
                 psr/parse
                 :parser/stmts)
      expr  (.expression (first stmts))]
  ;; (count (.arguments expr))
  ;; (instance? clox.ast.Variable (.callee expr))
  (.value (last (.arguments expr))))