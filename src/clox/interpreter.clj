(ns clox.interpreter
  (:require [clox.env :as env]
            [clox.error :refer [->RuntimeError]]
            [clox.callable :refer [->Clock]]
            [clox.function :refer [->LoxFunction ->Return]]))

(defn- stmts+ [intr v]
  (update intr :intr/stmts conj v))

(defn- env+ [intr v]
  (assoc intr :intr/env v))

(defn- globals+ [intr v]
  (assoc intr :intr/globals v))

(defn- sync-env "syncs env + globals to the intr" [intr env]
  (-> intr
      (env+ env)
      (globals+ env)))

(defmulti expr-visitor (fn [_ expr] (type expr)))
(defmulti stmt-visitor (fn [_ stmt] (type stmt)))

(defn- evaluate "evaluates expression" [intr expr]
  (.accept expr intr expr-visitor))

(defn- execute "executes statement" [intr stmt]
  (.accept stmt intr stmt-visitor))

(defn- equal? [a b]
  (cond
    (and (nil? a) (nil? b)) true
    (nil? a) false
    :else (= a b)))

(defn- !equal? [a b]
  (not (equal? a b)))

(defn- check-num-operand* [operator operand]
  (when-not (number? operand)
    (.panic! (->RuntimeError operator "operand must be a number."))))

(defn- check-num-operands*
  [operator & operands]
  (when-not (every? number? operands)
    (.panic! (->RuntimeError operator "operands must be a numbers."))))

(defmethod expr-visitor
  clox.ast.Literal
  [intr ^clox.ast.Literal expr]
  {:env  (:intr/env intr)
   :expr (.value expr)})

(defmethod expr-visitor
  clox.ast.Logical
  [intr ^clox.ast.Logical expr]
  (let [lefte (evaluate intr (.left expr))
        left  (:expr lefte)
        intr  (env+ intr (:env lefte))
        op    (:token/kind (.operator expr))
        or?   (= op :OR)
        left? (boolean left)]
    (cond
      (and or? left?)             lefte
      (and (not or?) (not left?)) lefte
      :else (evaluate intr (.right expr)))))

(defmethod expr-visitor
  clox.ast.Grouping
  [intr ^clox.ast.Grouping expr]
;; no need to return an expr map, because it evaluates an expression, which will return an expr map
  (evaluate intr (.expression expr)))

(defmethod expr-visitor
  clox.ast.Unary
  [intr ^clox.ast.Unary expr]
  (let [righte (evaluate intr (.right expr))
        right  (:expr righte)
        op     (.operator expr)]
    {:env  (:env righte)
     :expr (case (:token/kind op)
             :BANG  (not (boolean right))
             :MINUS (do
                      (check-num-operand* op right)
                      (- (double right)))
             nil)}))

(defmethod expr-visitor
  clox.ast.Variable
  [intr ^clox.ast.Variable expr]
  {:env  (:intr/env intr)
   :expr (env/pull (:intr/env intr) (.name expr))})

(defn- interpret-fn-args [intr ^clox.ast.Call expr]
  (let [base {:intr intr :args []}]
    (if-let [args-expr (not-empty (.arguments expr))]
      (let [xfn  (fn [acc arg-expr]
                   (let [exprI (evaluate (:intr acc) arg-expr)]
                     (-> acc
                         (update :intr env+ (:env exprI))
                         (update :args conj (:expr exprI)))))]
        (reduce xfn base args-expr))
      base)))

(defmethod expr-visitor
  clox.ast.Call
  [intr ^clox.ast.Call expr]
  (let [calleeN (:token/lexeme (.name (.callee expr)))
        calleeI (evaluate intr (.callee expr))
        ^clox.callable.ILoxCallable callee  (:expr calleeI)
        intr    (env+ intr (:env calleeI))
        intrpd  (interpret-fn-args intr expr)
        intr    (:intr intrpd)
        args    (:args intrpd)
        res     (if-not (instance? clox.callable.ILoxCallable  callee)
                  (->RuntimeError (.paren expr) "Can only call functions and classes.")
                  (.call callee intr args))
        callee  (:callee res)]
    {:env  (cond-> (:intr/env intr)
             (some? callee) (env/push calleeN callee))
     :expr (:expr res)}))

(defn- binary-operation [op left right]
  (if (= (:token/kind op) :PLUS)
    ;; if plus, then it can be two numbers
    (let [?? #(every? % [left right])]
      (cond
        (?? number?) (+ left right)
        (?? string?) (str left right)
        :else (.panic! (->RuntimeError op (str "Operands must be two numbers or two strings."
                                               "\n\t- left  : " (pr-str left)
                                               "\n\t- right : " (pr-str right))))))
    ;; if not plus, then it must be two numbers
    (do
      (check-num-operands* op left right)
      (case (:token/kind op)
        :GREATER        (> left right)
        :GREATER-EQUAL  (>= left right)
        :LESS           (< left right)
        :LESS-EQUAL     (<= left right)
        :BANG-EQUAL     (!equal? left right)
        :EQUAL-EQUAL    (equal? left right)
        :MINUS          (- left right)
        :SLASH          (double (/ left right))
        :STAR           (* left right)
        nil))))

(defmethod expr-visitor
  clox.ast.Binary
  [intr ^clox.ast.Binary expr]
  (let [lefte  (evaluate intr (.left expr))
        left   (:expr lefte)
        intr   (assoc intr :intr/env (:env lefte))
        righte (evaluate intr (.right expr))
        right  (:expr righte)
        op     (.operator expr)
        expr   (binary-operation op left right)]
    {:env  (:env righte)
     :expr expr}))

(defmethod expr-visitor
  clox.ast.Assign
  [intr ^clox.ast.Assign expr]
  (let [res   (evaluate intr (.value expr))
        value (:expr res)
        env   (:env res)]
    {:env  (env/assign env (.name expr) value)
     :expr value}))

(defmethod stmt-visitor
  clox.ast.Function
  [intr ^clox.ast.Function stmt]
  (let [env (:intr/env intr)
        lfn (->LoxFunction stmt env)
        env (env/push env (:token/lexeme (.name stmt)) lfn)]
    (-> intr
        (sync-env env)
        (stmts+ nil))))

(defmethod stmt-visitor
  clox.ast.Expression
  [intr ^clox.ast.Expression stmt]
  (let [res (evaluate intr (.expression stmt))]
    (-> intr
        (sync-env (:env res))
        (stmts+  (:expr res)))))

(defmethod stmt-visitor
  clox.ast.If
  [intr ^clox.ast.If stmt]
  (let [condie  (evaluate intr (.condition stmt))
        condi   (:expr condie)
        intr    (env+ intr (:env condie))
        then-br (.then-branch stmt)
        else-br (.else-branch stmt)
        res     (cond
                  condi   (execute intr then-br)
                  else-br (execute intr else-br)
                  :else   intr)]
    (-> intr
        (sync-env (:intr/env res))
        (stmts+ nil))))

(defmethod stmt-visitor
  clox.ast.While
  [intr ^clox.ast.While stmt]
  (let [res (loop [intr intr]
              (let [condie (evaluate intr (.condition stmt))
                    condi  (:expr condie)
                    intr   (env+ intr (:env condie))]
                (if (boolean condi)
                  (recur (execute intr (.body stmt)))
                  intr)))]
    (stmts+ res nil)))

(defmethod stmt-visitor
  clox.ast.Return
  [intr ^clox.ast.Return stmt]
  (let [vale (some->> (.value stmt) (evaluate intr))]
    ;; short circuiting. :c
    (.panic! (->Return intr (:expr vale)))))

(defmethod stmt-visitor
  clox.ast.Print
  [intr ^clox.ast.Print stmt]
  (let [res (evaluate intr (.expression stmt))]
    (-> intr
        (sync-env (:env res))
        (stmts+ (println (:expr res))))))

(defmethod stmt-visitor
  clox.ast.Var
  [intr ^clox.ast.Var stmt]
  (let [v (some->> (.initializer stmt) (evaluate intr))
        k (:token/lexeme (.name stmt))
        e (env/push (or (:env v) (:intr/env intr)) k (:expr v))]
    (-> intr
        (sync-env e)
        (stmts+ nil))))

(defmethod stmt-visitor
  clox.ast.Block
  [intr ^clox.ast.Block stmt]
  (let [stmts (.statements stmt)
        encl  (env/env:new :env/enclosing (:intr/env intr))
        base  (assoc intr :intr/env encl)
        exe   (fn [intr stmt] (execute intr stmt))
        intrn (reduce exe base stmts)
        env   (:env/enclosing (:intr/env intrn))]
    (-> intrn
        (sync-env env)
        (stmts+ nil))))

(defn interpreter:new
  [stmts & {env :env}]
  {:intr/env            (or (not-empty env) (env/env:new))
   :intr/runtime-error? false
   :intr/stmts          stmts
   :intr/globals        env
   :intr/execute        execute
   :intr/evaluate       evaluate
   :intr/errors         []})

(defn interpreter [{env :intr/env :as intr}]
  (sync-env intr (env/push env "clock" (->Clock))))

(defn interpret [intr]
  (try
    (let [stmts (:intr/stmts intr)
          base  (assoc intr :intr/stmts [])
          exe   (fn [intr stmt] (execute intr stmt))]
      (reduce exe base stmts))
    (catch Exception e
      (println "ERROR:" (ex-message e))
      (-> intr
          (assoc :intr/runtime-error? true)
          (update :intr/errors conj e)))))
