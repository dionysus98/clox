(ns clox.parser
  (:require [clox.ast :as ast]
            [clox.error :refer [->ParserError]]))

(defn expr+ [psr expr]
  (assoc psr :parser/expr expr))

(defn stmt+ [psr stmt]
  (assoc psr :parser/stmt stmt))

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

(defn !check? ^Boolean [psr token-kind]
  (not (check? psr token-kind)))

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
    (.panic! (->ParserError psr info))))

(defn consume! [psr token-kind msg]
  (if (check? psr token-kind)
    (adv psr)
    (error! psr msg)))

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
      (?? psr
          :NUMBER
          :STRING)    (p> psr (ast/->Literal (:token/literal (pk psr))))
      (?? psr :THIS)  (p> psr (ast/->This (pk psr)))
      (?? psr :IDENT) (p> psr (ast/->Variable (pk psr)))
      (?? psr
          :LEFT-PAREN) (let [psr (-> (parser :expression (adv psr))
                                     (consume! :RIGHT-PAREN "Expect ')' after expression."))]
                         (expr+ psr (ast/->Grouping (:parser/expr psr))))
      :else (error! psr "Expected Expression"))))

(defn- <parse-fn-args [callee-]
  (if (!check? callee- :RIGHT-PAREN)
    (loop [args- []
           psr-  (parser :expression callee-)]
      (if (>= (count args-) 255)
        (error! psr- "Can't have more than 255 arguments.")
        (let [?>    (match psr- :COMMA)
              args- (conj args- psr-)]
          (cond
            ?>    (recur args- (parser :expression (adv psr-)))
            :else [(mapv :parser/expr args-) psr-]))))
    [[] callee-]))

(defn- finish-call [callee-]
  (let [[args psr-] (<parse-fn-args callee-)
        paren-      (consume! psr- :RIGHT-PAREN "Expect ')' after arguments.")
        expr        (ast/->Call (:parser/expr callee-) (:parser/expr paren-) args)]
    (expr+ paren- expr)))

(defmethod parser :call [_ psr]
  (let [psr- (parser :primary psr)]
    (loop [psr- psr-]
      (cond
        (match psr- :LEFT-PAREN) (recur (finish-call (adv psr-)))
        (match psr- :DOT)
        (recur (let [next- (adv psr-)]
                 (-> next-
                     (consume! :IDENT "Expect property name after '.'")
                     (expr+ (ast/->Get (:parser/expr psr-) (pk next-))))))
        :else psr-))))

(defmethod parser :unary [_ psr]
  (if (match psr :BANG :MINUS)
    (let [psr-   (adv psr)
          op     (prev psr-)
          right- (parser :unary psr-)
          expr   (ast/->Unary op (:parser/expr right-))]
      (expr+ right- expr))
    (parser :call psr)))

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
                  (let [left   (:parser/expr psr-)
                        psr-   (adv psr-)
                        op     (prev psr-)
                        right- (parser :factor psr-)
                        expr   (ast/->Binary left op (:parser/expr right-))]
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
                        expr   (ast/->Binary expr op (:parser/expr right-))]
                    (>parse< (expr+ right- expr)))
                  psr-))]
    (parse (parser :term psr))))

(defmethod parser :equality [_ psr]
  (let [parse (fn >parse< [psr-]
                (if (match psr- :BANG-EQUAL :EQUAL-EQUAL)
                  (let [psr-   (adv psr-)
                        op     (prev psr-)
                        right- (parser :comparison psr-)
                        expr   (ast/->Binary (:parser/expr psr-) op (:parser/expr right-))]
                    (>parse< (expr+ right- expr)))
                  psr-))]
    (parse (parser :comparison psr))))

(defmethod parser :and [_ psr]
  (let [parse (fn >parse< [psr-]
                (if (match psr- :AND)
                  (let [psr-   (adv psr-)
                        op     (prev psr-)
                        right- (parser :equality psr-)
                        expr   (ast/->Logical (:parser/expr psr-) op (:parser/expr right-))]
                    (>parse< (expr+ right- expr)))
                  psr-))]
    (parse (parser :equality psr))))

(defmethod parser :or [_ psr]
  (let [parse (fn >parse< [psr-]
                (if (match psr- :OR)
                  (let [psr-   (adv psr-)
                        op     (prev psr-)
                        right- (parser :and psr-)
                        expr   (ast/->Logical (:parser/expr psr-) op (:parser/expr right-))]
                    (>parse< (expr+ right- expr)))
                  psr-))]
    (parse (parser :and psr))))

(defmethod parser :assignment [_ psr]
  (let [parse (fn >parse< [psr-]
                (if (match psr- :EQUAL)
                  (let [expr   (:parser/expr psr-)
                        psr-   (adv psr-)
                        value- (parser :assignment psr-)
                        expr   (cond
                                 (instance? clox.ast.Variable expr)
                                 (ast/->Assign (.name expr) (:parser/expr value-))
                                 (instance? clox.ast.Get expr)
                                 (ast/->Set (.object expr) (.name expr) (:parser/expr value-))
                                 :else (error! value- "Invalid assignment target."))]
                    (>parse< (expr+ value- expr)))
                  psr-))]
    (parse (parser :or psr))))

(defmethod parser :expression [_ psr]
  (parser :assignment psr))

(defmethod parser :return [_ psr]
  (let [kw    (pk psr)
        expr- (when (!check? psr :SEMICOLON)
                (parser :expression psr))
        psr-  (-> (or expr- psr)
                  (consume! :SEMICOLON "Expect ';' after value."))
        value (:parser/expr expr-)]
    (stmt+ psr- (ast/->Return kw value))))

(defmethod parser :print [_ psr]
  (let [psr-  (-> (parser :expression psr)
                  (consume! :SEMICOLON "Expect ';' after value."))
        value (:parser/expr psr-)]
    (stmt+ psr- (ast/->Print value))))

(defmethod parser :expr-stmt [_ psr]
  (let [psr- (-> (parser :expression psr)
                 (consume! :SEMICOLON "Expect ';' after value."))
        expr (:parser/expr psr-)]
    (stmt+ psr- (ast/->Expression expr))))

(defmethod parser :block [_ psr]
  (let [[psr- stmts-] ;;
        (loop [psr-   psr
               stmts- []]
          (if (and (!check? psr- :RIGHT-BRACE)
                   (!end? psr-))
            (let [psr- (parser :declaration psr-)]
              (recur psr- (conj stmts- psr-)))
            [psr- (mapv :parser/stmt stmts-)]))]
    (-> psr-
        (consume! :RIGHT-BRACE "Expect '}' after block.")
        (stmt+    (ast/->Block stmts-)))))

(defmethod parser :if [_ psr]
  (let [psr-    (consume! psr :LEFT-PAREN "Expect '(' after 'if'.")
        expr-   (-> (parser :expression psr-)
                    (consume! :RIGHT-PAREN "Expect ')' after 'if' condition."))
        then-   (parser :statement expr-)
        else-   (when (match then- :ELSE)
                  (parser :statement (adv then-)))
        if-stmt (ast/->If
                 (:parser/expr expr-)
                 (:parser/stmt then-)
                 (:parser/stmt else-))]
    (stmt+ (or else- then-) if-stmt)))

(defmethod parser :while [_ psr]
  (let [psr-  (consume! psr :LEFT-PAREN "Expect '(' after 'while'.")
        expr- (-> (parser :expression psr-)
                  (consume! :RIGHT-PAREN "Expect ')' after 'while' condition."))
        body- (parser :statement expr-)
        whst  (ast/->While (:parser/expr expr-)
                           (:parser/stmt body-))]
    (stmt+ body- whst)))

(defmethod parser :for [_ psr]
  (let [psr-  (consume! psr :LEFT-PAREN "Expect '(' after 'for'.")
        init- (let [?? (partial match psr-)]
                (cond
                  (?? :SEMICOLON) nil
                  (?? :VAR) (parser :var (adv psr-))
                  :else  (parser :expr-stmt (adv psr-))))
        psr-  (or init- (adv psr-))
        cond- (when (!check? psr- :SEMICOLON) (parser :expression psr-))
        psr-  (-> (or cond- psr-)
                  (consume! :SEMICOLON "Expect ';' after loop condition."))
        inc-  (when (!check? psr- :RIGHT-PAREN) (parser :expression psr-))
        psr-  (-> (or inc- psr-)
                  (consume! :RIGHT-PAREN "Expect ')' after 'for' clauses."))
        body- (parser :statement psr-)]
    (as-> body- $
      (if inc-
        (stmt+ $ (ast/->Block [(:parser/stmt $) (ast/->Expression (:parser/expr inc-))]))
        $)
      (if-not cond-
        (stmt+ $ (ast/->While (ast/->Literal true) (:parser/stmt $)))
        (stmt+ $ (ast/->While (:parser/expr cond-) (:parser/stmt $))))
      (if init-
        (stmt+ $ (ast/->Block [(:parser/stmt init-) (:parser/stmt $)]))
        $))))

(defmethod parser :statement [_ psr]
  (let [?? (partial match psr)
        >> #(parser % (adv psr))]
    (cond
      (?? :FOR)        (>> :for)
      (?? :IF)         (>> :if)
      (?? :PRINT)      (>> :print)
      (?? :RETURN)     (>> :return)
      (?? :WHILE)      (>> :while)
      (?? :LEFT-BRACE) (>> :block)
      :else (parser :expr-stmt psr))))

(defmethod parser :var [_ psr]
  (let [psr- (consume! psr :IDENT "Expect variable name.")
        psr- (if (match psr- :EQUAL)
               (parser :expression (adv psr-))
               (assoc psr- :parser/expr (ast/->Literal nil)))
        psr- (consume! psr- :SEMICOLON "Expect ';' after variable declaration.")
        expr (:parser/expr psr-)
        varn (pk psr)]
    (stmt+ psr- (ast/->Var varn expr))))

(defn- parse-fn-params [psr-]
  (if (!check? psr- :RIGHT-PAREN)
    (loop [params- []
           psr-    (consume! psr- :IDENT "Expect parameter name.")]
      (if (>= (count params-) 255)
        (error! (prev psr-) "Can't have more than 255 parameters.")
        (let [?>    (match psr- :COMMA)
              args- (conj params- psr-)]
          (cond
            ?>    (recur args- (consume! (adv psr-) :IDENT "Expect parameter name."))
            :else [(mapv prev args-) psr-]))))
    [[] psr-]))

(defmethod parser :function
  [_ {method? :method?
      :as     psr}]
  (let [kind   (if method? "method" "function")
        name-  (-> (dissoc psr :method?)
                   (consume! :IDENT (str "Expect " kind " name.")))
        parsed (-> name-
                   (consume! :LEFT-PAREN (str "Expect '(' after " kind " name."))
                   parse-fn-params)
        params (first parsed)
        psr-   (-> (second parsed)
                   (consume! :RIGHT-PAREN "Expect ')' after parameters.")
                   (consume! :LEFT-BRACE "Expect '{' before function body."))
        body-  (parser :block psr-)
        stmt   (ast/->Function (prev name-) params (:parser/stmt body-))]
    (stmt+ body- stmt)))

(defmethod parser :class [_ psr]
  (let [name-          (consume! psr :IDENT "Expect class name.")
        parsed         (consume! name- :LEFT-BRACE "Expect '{' after class body.")
        [psr- methods] ;;
        (loop [psr-    parsed
               methods []]
          (if (and (!check? psr- :RIGHT-BRACE) (!end? psr))
            (let [psr- (parser :function (assoc psr- :method? true))]
              (recur psr- (conj methods psr-)))
            [psr- (mapv :parser/stmt methods)]))
        end-           (consume! psr- :RIGHT-BRACE "Expect '}' after class body.")
        stmt           (ast/->LoxClass (prev name-) methods)]
    (stmt+ end- stmt)))

(defmethod parser :declaration [_ psr]
  (try
    (cond
      (match psr :CLASS) (parser :class (adv psr))
      (match psr :FUN)   (parser :function (adv psr))
      (match psr :VAR)   (parser :var (adv psr))
      :else (parser :statement psr))
    (catch Exception e
      (println (ex-message e))
      (synchronize psr))))

(defmethod parser :parser/had-error? [_ psr]
  psr)

(defn parse
  ([psr] (parse psr []))
  ([psr statements]
   (if (!end? psr)
     (let [psr- (parser :declaration psr)]
       (parse psr- (conj statements psr-)))
     (assoc psr :parser/stmts (mapv :parser/stmt statements)))))

(defn parser:new [tokens]
  {:parser/current    0
   :parser/tokens     tokens
   :parser/errors     []
   :parser/had-error? false})
