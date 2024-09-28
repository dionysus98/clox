(ns clox.interpreter
  (:require [clox.env :as env]
            [clox.error :refer [->RuntimeError]]))

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

(defmethod expr-visitor :literal [_ _ expr]
  (.value expr))

(defmethod expr-visitor :grouping [_ intr expr]
  (evaluate intr (.expression expr)))

(defmethod expr-visitor :unary [_ intr expr]
  (let [right (evaluate intr (.right expr))
        op    (.operator expr)]
    (case (:token/kind op)
      :BANG  (not (boolean right))
      :MINUS (do
               (check-num-operand* op right)
               (- (double right)))
      nil)))

(defmethod expr-visitor :variable [_ intr expr]
  (env/pull (:interpreter/env intr) (.name expr)))

(defmethod expr-visitor :binary [_ intr expr]
  (let [left   (evaluate intr (.left expr))
        right  (evaluate intr (.right expr))
        op     (.operator expr)
        check* (fn [] (check-num-operands* op left right))]
    (case (:token/kind op)
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
      nil)))

(defmethod stmt-visitor :expression [_ intr stmt]
  (stmts+ intr (evaluate intr (.expression stmt))))

(defmethod stmt-visitor :print
  [_ intr stmt]
  (stmts+ intr (println (evaluate intr (.expression stmt)))))

(defmethod stmt-visitor :var
  [_ intr stmt]
  (let [v (some->> (.initializer stmt) (evaluate intr))
        k (:token/lexeme (.name stmt))
        e (env/push (:interpreter/env intr) k v)]
    (println (str "#" k))
    (-> intr
        (env+ e)
        (stmts+ nil))))

(defn interpreter:new
  [stmts & {values :values
            env    :env}]
  {:interpreter/env            (or (not-empty env) (env/env:new :values values))
   :interpreter/runtime-error? false
   :interpreter/stmts          stmts
   :interpreter/errors         []})

(defn interpret [intr]
  (try
    (let [stmts (:interpreter/stmts intr)
          base  (assoc intr :interpreter/stmts [])
          exe   (fn [intr stmt] (execute intr stmt))]
      (reduce exe base stmts))
    (catch Exception e
      (println "ERROR: " (ex-message e))
      (-> intr
          (assoc :interpreter/runtime-error? true)
          (update :interpreter/errors conj e))))) 
