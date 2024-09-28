(ns clox.main-test
  (:require [clojure.test :refer [deftest is testing]]
            [clox.ast :as ast]
            [clox.ast-test :as printer]
            [clox.token :as t]))

(deftest ast-test
  (testing "testsing AST generation."
    #_{:clj-kondo/ignore [:unresolved-var]}
    (let [left  (ast/->Unary
                 (t/token:new :MINUS "-" nil 1)
                 (ast/->Literal 123))
          op    (t/token:new :STAR "*" nil 1)
          right (ast/->Grouping (ast/->Literal 45.67))
          bin   (ast/->Binary left op right)
          ast   (printer/->AstPrinter bin)
          env   {}]
      (is (= (.print! ast env) "(* (- 123) (group 45.67))")))))
