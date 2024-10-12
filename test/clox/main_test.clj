(ns clox.main-test
  (:require [clojure.test :refer [deftest is testing]]
            [clox.ast :as ast]
            [clox.ast-test :as printer]
            [clox.interpreter :as intr]
            [clox.lexer :as lex]
            [clox.parser :as psr]
            [clox.token :as t]))

(deftest ast-test
  (testing "testsing AST generation."
    (let [left  (ast/->Unary
                 (t/token:new :MINUS "-" nil 1)
                 (ast/->Literal 123))
          op    (t/token:new :STAR "*" nil 1)
          right (ast/->Grouping (ast/->Literal 45.67))
          bin   (ast/->Binary left op right)
          ast   (printer/->AstPrinter bin)
          env   {}]
      (is (= (.print! ast env) "(* (- 123) (group 45.67))")))))

(deftest basic-print-test
  (testing "Basic print statement | tokens, statements"
    (let [src   "var a = (7 + 8) * (7 * (8 + 9));
                 print a;"
          tks   (->> src
                     lex/lexer:new
                     lex/lex
                     :lexer/tokens)
          stmts (->> tks
                     psr/parser:new
                     psr/parse
                     :parser/stmts)]
      (is (= 23 (count tks)))
      (is (= 1 (:token/line (first tks))))
      (is (= 2 (:token/line (last tks))))
      (is (= :VAR (:token/kind (first tks))))
      (is (= :EOF (:token/kind (last tks))))
      (is (= 2 (count stmts)))
      (is (instance? clox.ast.Var (first stmts)))
      (is (instance? clox.ast.Print (last stmts)))
      (testing "Basic print statement | interpret"
        (let [!print-op (atom nil)]
          (binding [*out* (proxy [java.io.Writer] []
                            (close [] nil)
                            (flush [] nil)
                            (write
                              ([cbuf] (swap! !print-op #(or % cbuf)))
                              ([cbuf off len] nil)))]
            (let [intr (intr/interpret (intr/interpreter:new stmts))]
              (is (= (:env/values (:interpreter/env intr)) {"a" 1785.0}))
              (is (empty? (:interpreter/errors intr)))
              (is (false? (:interpreter/runtime-error? intr)))
              (is (= "1785.0" @!print-op)))))))))

(deftest if-statement-test
  (testing "Basic if statement"
    (let [src   "var a = 5;
                 var b = 8;
                 print a + b; // 13.0

                 if (true) {
                     var a = 8;
                     print a + b; // 16.0
                 }

                 print a + b; // 13.0

                  if (false) {
                      b = a;
                  } else {
                     b = 15;
                  }

                 print a + b; // 20.0;"
          tks   (->> src
                     lex/lexer:new
                     lex/lex
                     :lexer/tokens)
          stmts (->> tks
                     psr/parser:new
                     psr/parse
                     :parser/stmts)]
      (testing "Basic print statement | interpret"
        (let [!print-op (atom [])]
          (binding [*out* (proxy [java.io.Writer] []
                            (close [] nil)
                            (flush [] nil)
                            (write
                              ([cbuf] (when (not= cbuf "\n")
                                        (swap! !print-op conj cbuf)))
                              ([cbuf off len] nil)))]
            (let [intr (intr/interpret (intr/interpreter:new stmts))]
              (is (= (:env/values (:interpreter/env intr)) {"a" 5.0 "b" 15.0}))
              (is (empty? (:interpreter/errors intr)))
              (is (false? (:interpreter/runtime-error? intr)))
              (is (= ["13.0" "16.0" "13.0" "20.0"] @!print-op)))))))))

(comment
  (test #'ast-test)
  :rcf)

