(ns clox.parser
  (:require [clox.ast :as ast]))

(defn expr+ [psr expr]
  (assoc psr :parser/expr expr))

(defn pk "current token" [psr]
  (nth (:parser/tokens psr) (:parser/current psr)))

(defn prev "prev token" [psr]
  (nth (:parser/tokens psr) (dec (:parser/current psr))))

(defn end? "is at end" ^Boolean [psr]
  (= (:token/kind (pk psr)) :EOF))

(defn !end? "is not at end" ^Boolean [psr]
  (not (end? psr)))

(defn adv "advance" [psr]
  (cond-> psr
    (!end? psr) (update :parser/current inc)))

(defn check? "check if current token is the given token"
  ^Boolean [psr token-kind]
  (if (end? psr)
    false
    (= (:token/kind (pk psr)) token-kind)))

(defn match ^Boolean [psr & token-kinds]
  (cond
    (empty? token-kinds) false
    (check? psr (first token-kinds)) true
    :else (apply match psr (rest token-kinds))))

(defn error! [psr cause]
  (let [info {:type  :parser-error
              :ctx   (pk psr)
              :cause cause}
        psr  (-> psr
                 (assoc :parser/had-error? true)
                 (update :parser/errors conj info))]
    (throw (ex-info cause psr))))

(defn consume! [psr token-kind msg]
  (if (check? psr token-kind)
    (adv psr)
    (error! psr msg)))

(defmulti parser
  (fn [rule psr]
    (if (:parser/had-error? psr)
      :parser/had-error?
      rule)))

(defmethod parser :primary [_ psr]
  (let [?? (fn [psr & kinds] (apply match psr kinds))
        p> (fn [psr expr] (-> (adv psr) (expr+ expr)))]
    (cond
      (?? psr :FALSE) (p> psr (ast/->Literal false))
      (?? psr :TRUE)  (p> psr (ast/->Literal true))
      (?? psr :NIL)   (p> psr (ast/->Literal nil))
      (?? psr :NUMBER :STRING) (p> psr (ast/->Literal (:token/literal (pk psr))))
      (?? psr :LEFT-PAREN) (let [psr (-> (parser :expression (adv psr))
                                         (consume! :RIGHT-PAREN "Expect ')' after expression."))]
                             (assoc psr :parser/expr (ast/->Grouping (:parser/expr psr))))
      :else (error! psr "Expected Expression"))))

(defmethod parser :unary [_ psr]
  (if (match psr :BANG :MINUS)
    (let [psr-   (adv psr)
          op     (prev psr-)
          right- (parser :unary psr-)
          expr   (ast/->Unary op (:parser/expr right-))]
      (expr+ right- expr))
    (parser :primary psr)))

(defmethod parser :factor [_ psr]
  (let [parse (fn >parse< [psr-]
                (if (match psr- :SLASH :STAR)
                  (let [expr  (:parser/expr psr-)
                        psr-  (adv psr-)
                        op    (prev psr-)
                        right- (parser :unary psr-)
                        expr  (ast/->Binary expr op (:parser/expr right-))]
                    (>parse< (expr+ right- expr)))
                  psr-))]
    (parse (parser :unary psr))))

(defmethod parser :term [_ psr]
  (let [parse (fn >parse< [psr-]
                (if (match psr- :MINUS :PLUS)
                  (let [expr   (:parser/expr psr-)
                        psr-   (adv psr-)
                        op     (prev psr-)
                        right- (parser :factor psr)
                        expr   (ast/->Binary expr op (:parser/right right-))]
                    (>parse< (expr+ right- expr)))
                  psr-))]
    (parse (parser :factor psr))))

(defmethod parser :comparison [_ psr]
  (let [parse (fn >parse< [psr-]
                (if (match psr-
                      :GREATER :GREATER-EQUAL
                      :LESS     :LESS-EQUAL)
                  (let [expr   (:parser/expr psr-)
                        psr-   (adv psr-)
                        op     (prev psr-)
                        right- (parser :term psr-)
                        expr   (ast/->Binary expr op (:parser/right right-))]
                    (>parse< (expr+ right- expr)))
                  psr-))]
    (parse (parser :term psr))))

(defmethod parser :equality [_ psr]
  (let [parse (fn >parse< [psr-]
                (if (match psr- :BANG-EQUAL :EQUAL-EQUAL)
                  (let [expr  (:parser/expr psr-)
                        psr-  (adv psr-)
                        op    (prev psr-)
                        right- (parser :comparison psr-)
                        expr  (ast/->Binary expr op (:parser/right right-))]
                    (>parse< (expr+ right- expr)))
                  psr-))]
    (parse (parser :comparison psr))))

(defmethod parser :expression [_ psr]
  (parser :equality psr))

(defmethod parser :parser/had-error? [_ psr]
  psr)

(defn parse [psr]
  (try
    (parser :expression psr)
    (catch NullPointerException e (println e))
    (catch Exception e (ex-data e))))

(defn synchronize [psr]
  (let [prev? (fn [psr- tk] (= (:token/kind (prev psr-)) tk))
        pk?   (fn [psr-]
                (some #{(:token/kind (pk psr-))}
                      #{:CLASS
                        :FUN
                        :VAR
                        :FOR
                        :IF
                        :WHILE
                        :PRINT
                        :RETURN}))]
    (loop [psr- (adv psr)]
      (cond
        (end? psr-) psr-
        (prev? psr- :SEMICOLON) psr-
        (pk? psr-) psr-
        :else (recur (adv psr-))))))

(defn parser:new [tokens]
  {:parser/current    0
   :parser/tokens     tokens
   :parser/errors     []
   :parser/had-error? false})

(comment
  (let [tokens3 [#:token{:kind    :LEFT-PAREN
                         :lexeme  "("
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
                         :line    1}]]
    (parse (parser:new tokens3)))
  :rcf)
