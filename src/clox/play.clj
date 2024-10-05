 (ns clox.play
  (:require [clox.interpreter :as intr]
            [clox.lexer :as lex]
            [clox.parser :as psr]))

(let [print-op nil]
  (binding [*out* (proxy [java.io.Writer] []
                    (close [] nil)
                    (flush [] nil)
                    (write
                      ([cbuf] (when-not print-op (set! print-op cbuf)))
                      ([cbuf off len] nil)))]
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
      (intr/interpret intr)
      print-op)))


(with-out-str (println "a"))

*out*

java.io.Reader