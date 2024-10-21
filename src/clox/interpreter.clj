(ns clox.interpreter
  (:require [clox.env :as env]
            [clox.error :refer [->RuntimeError]]
            [clox.callable :refer [->Clock ILoxCallable]]))

(defn stmts+ [intr v]
  (update intr :interpreter/stmts conj v))

(defn env+ [intr v]
  (assoc intr :interpreter/env v))

(defn stmt+ [intr v]
  (assoc intr :interpreter/stmt v))

(defn expr+ [intr v]
  (assoc intr :interpreter/expr v))

(defmulti expr-visitor (fn [type _ _] type))
(defmulti stmt-visitor (fn [type _ _] type))

(defn evaluate "evaluates expression" [intr expr]
  (.accept expr intr expr-visitor))

(defn execute "executes statement" [intr stmt]
  (.accept stmt intr stmt-visitor))

(deftype LoxFunction [declaration]
  ILoxCallable
  (arity [_] (count (.params declaration)))
  (call [_ intr args]
    (let [env (:env (reduce
                     (fn [acc param]
                       (-> acc
                           (update :env env/push (:token/lexeme param) (nth args (:i acc)))
                           (update :i inc)))
                     {:i   0
                      :env (env/env:new (:interpreter/globals intr))}
                     (.params declaration)))]
      (execute (env+ intr env) (.body declaration))))
  Object
  (toString [_] (str "<fn " (-> declaration .name :token/lexeme) ">")))

(defn equal? [a b]
  (cond
    (and (nil? a) (nil? b)) true
    (nil? a) false
    :else (= a b)))

(defn !equal? [a b]
  (not (equal? a b)))

(defn check-num-operand* [operator operand]
  (when-not (number? operand)
    (.panic! (->RuntimeError operator "operand must be a number."))))

(defn check-num-operands*
  [operator & operands]
  (when-not (every? number? operands)
    (.panic! (->RuntimeError operator "operands must be a numbers."))))

(defmethod expr-visitor :literal [_ intr expr]
  {:env  (:interpreter/env intr)
   :expr (.value expr)})

(defmethod expr-visitor :logical
  [_ intr ^clox.ast.Logical expr]
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

(defmethod expr-visitor :grouping [_ intr expr]
;; no need to return an expr map, because it evaluates an expression, which will return an expr map
  (evaluate intr (.expression expr)))

(defmethod expr-visitor :unary [_ intr expr]
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

(defmethod expr-visitor :variable [_ intr expr]
  {:env  (:interpreter/env intr)
   :expr (env/pull (:interpreter/env intr) (.name expr))})

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

(defmethod expr-visitor :call
  [_ intr ^clox.ast.Call expr]
  (let [calleeI (evaluate intr (.callee expr))
        ^clox.callable.ILoxCallable callee  (:expr calleeI)
        intr    (env+ intr (:env calleeI))
        res     (interpret-fn-args intr expr)
        intr    (or (:intr res) intr)
        args    (:args res)
        res     (if-not (instance? clox.callable.ILoxCallable  callee)
                  (->RuntimeError (.paren expr) "Can only call functions and classes.")
                  (.call callee intr args))]
    {:env  (:interpreter/env intr)
     :expr (:interpreter/expr res)}))

(defmethod expr-visitor :binary [_ intr expr]
  (let [lefte  (evaluate intr (.left expr))
        left   (:expr lefte)
        intr   (assoc intr :interpreter/env (:env lefte))
        righte (evaluate intr (.right expr))
        right  (:expr righte)
        op     (.operator expr)
        check* (fn [] (check-num-operands* op left right))
        expr   (case (:token/kind op)
                 :GREATER (do (check*) (> left right))
                 :GREATER-EQUAL (do (check*) (>= left right))
                 :LESS (do (check*) (< left right))
                 :LESS-EQUAL  (do (check*) (<= left right))
                 :BANG-EQUAL  (!equal? left right)
                 :EQUAL-EQUAL (equal? left right)
                 :MINUS (do (check*) (- left right))
                 :SLASH (do (check*) (double (/ left right)))
                 :STAR  (do (check*) (* left right))
                 :PLUS  (let [?? #(every? % [left right])]
                          (cond
                            (?? number?) (+ left right)
                            (?? string?) (str left right)
                            :else (.panic! (->RuntimeError op (str "Operands must be two numbers or two strings."
                                                                   "\n\t- left  : " (pr-str left)
                                                                   "\n\t- right : " (pr-str right))))))
                 nil)]
    {:env  (:env righte)
     :expr expr}))

(defmethod expr-visitor :assign
  [_ intr ^clox.ast.Assign expr]
  (let [res   (evaluate intr (.value expr))
        value (:expr res)
        env   (:env res)]
    {:env  (env/assign env (.name expr) value)
     :expr value}))

(defmethod stmt-visitor :function
  [_ intr ^clox.ast.Function stmt]
  (let [lfn  (->LoxFunction stmt)
        intr (update intr :interpreter/env
                     env/push (:token/lexeme (.name stmt)) lfn)]
    (stmts+ intr nil)))

(defmethod stmt-visitor :expression
  [_ intr ^clox.ast.Expression stmt]
  (let [res (evaluate intr (.expression stmt))]
    (-> intr
        (env+ (:env res))
        (stmts+  (:expr res)))))

(defmethod stmt-visitor :if
  [_ intr ^clox.ast.If stmt]
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
        (env+ (:interpreter/env res))
        (stmts+ nil))))

(defmethod stmt-visitor :while
  [_ intr ^clox.ast.While stmt]
  (let [res (loop [intr intr]
              (let [condie (evaluate intr (.condition stmt))
                    condi  (:expr condie)
                    intr   (env+ intr (:env condie))]
                (if (boolean condi)
                  (recur (execute intr (.body stmt)))
                  intr)))]
    (stmts+ res nil)))

(defmethod stmt-visitor :print
  [_ intr ^clox.ast.Print stmt]
  (let [res (evaluate intr (.expression stmt))]
    (-> intr
        (env+  (:env res))
        (stmts+ (println (:expr res))))))

(defmethod stmt-visitor :var
  [_ intr stmt]
  (let [v (some->> (.initializer stmt) (evaluate intr))
        k (:token/lexeme (.name stmt))
        e (env/push (or (:env v) (:interpreter/env intr)) k (:expr v))]
    (-> intr
        (env+ e)
        (stmts+ nil))))

(defmethod stmt-visitor :block
  [_ {env :interpreter/env
      :as intr} stmt]
  (let [stmts (.statements stmt)
        encl  (env/env:new :env/enclosing env)
        base  (assoc intr :interpreter/env encl)
        exe   (fn [intr stmt] (execute intr stmt))
        intr  (reduce exe base stmts)
        env   (:env/enclosing (:interpreter/env intr))]
    (-> intr
        (env+ env)
        (stmts+ nil))))

(defn interpreter:new
  [stmts & {values :values
            env    :env}]
  (let [env (or (not-empty env)
                (env/env:new :values values))]
    {:interpreter/env            env
     :interpreter/runtime-error? false
     :interpreter/stmts          stmts
     :interpreter/globals        (env/env:new)
     :interpreter/errors         []}))

(defn interpreter [intr]
  (update intr :interpreter/globals env/push "clock" (->Clock)))

(defn interpret [intr]
  (try
    (let [stmts (:interpreter/stmts intr)
          base  (assoc intr :interpreter/stmts [])
          exe   (fn [intr stmt] (execute intr stmt))]
      (reduce exe base stmts))
    (catch Exception e
      (println "ERROR:" (ex-message e))
      (-> intr
          (assoc :interpreter/runtime-error? true)
          (update :interpreter/errors conj e)))))
