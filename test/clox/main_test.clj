(ns clox.main-test
  (:require [clojure.test :refer [deftest is testing]]
            [clox.ast :as ast]
            [clox.ast-test :as printer]            
            [clox.token :as t]
            [clox.parser :as psr]))

(deftest ast-test
  (testing "testsing AST generation."
    #_{:clj-kondo/ignore [:unresolved-var]}
    (let [left  (ast/->Unary
                 (t/token:new :MINUS "-" nil 1)
                 (ast/->Literal 123))
          op    (t/token:new :STAR "*" nil 1)
          right (ast/->Grouping (ast/->Literal 45.67))
          bin   (ast/->Binary left op right)
          ast   (printer/->AstPrinter bin)]
      (is (= (printer/print! ast) "(* (- 123) (group 45.67))")))))



(comment
  (printer/print! (printer/->AstPrinter (ast/->Grouping (ast/->Literal 45.67))))

  (let [tokens3 [#:token{:kind    :VAR
                         :lexeme  "var"
                         :literal nil
                         :line    1}
                 #:token{:kind    :LEFT-PAREN
                         :lexeme  "("
                         :literal nil
                         :line    1}
                 #:token{:kind    :MINUS
                         :lexeme  "-"
                         :literal nil
                         :line    1}
                 #:token{:kind    :NUMBER
                         :lexeme  "123"
                         :literal 123
                         :line    1}
                 #:token{:kind    :RIGHT-PAREN
                         :lexeme  ")"
                         :literal nil
                         :line    1}
                 #:token{:kind    :EOF
                         :lexeme  ""
                         :literal nil
                         :line    1}]
        expr    (:parser/expr (psr/parse (psr/parser:new tokens3)))]
    ;; (printer/print! (printer/->AstPrinter expr))
    (psr/parse (psr/parser:new tokens3))
    )


  :rcf)
