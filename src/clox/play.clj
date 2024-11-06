 (ns clox.play
  (:require [clox.interpreter :as intr]
            [clox.lexer :as lex]
            [clox.parser :as psr]))

(def ^:private tokenize
  "given the `src as string` returns lexed `tokens`"
  (comp :lexer/tokens lex/lex lex/lexer:new))

(def ^:private parse
  "given `tokens` returns **parsed** `statements`"
  (comp :parser/stmts psr/parse psr/parser:new))

(def ^:private interpret!
  "given `statements` interprets and returns `interpreter`"
  (comp intr/interpret intr/interpreter intr/interpreter:new))

(def ^:private lox!
  "given the `src as string` do:
   - `tokenize` -> `parse` -> `interpret`"
  (comp interpret! parse tokenize))

(lox! "for (var a = 10; a > 0; a = a - 2) { print a; }")

(lox! "var a = (7 + 8) * (7 * (8 + 9)); 
       print a;")

(let [src   "event(name, args, 5);"
      stmts (-> src tokenize parse)
      expr  (.expression (first stmts))]
  [(count (.arguments expr))
   (instance? clox.ast.Variable (.callee expr))
   (.value (last (.arguments expr)))])

(let [src   "fun abc(a, b, c) {
                print a + b + c;
             }"
      stmts (-> src tokenize parse)
      fun (first stmts)]
  [(.name fun) (.params fun) (.body fun)])

(let [src   "var AH = 500;
             {    
                 fun showA() {
                 print AH;
                 return AH;
                 }
                 
                 showA();    
             }
             var k;"
      stmts (-> src tokenize parse)]
  (.initializer (last stmts)))
