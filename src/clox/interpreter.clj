(ns clox.interpreter
  (:require [clox.env :as env]
            [clox.error :refer [->RuntimeError]]))

(defn stmts+ [intr v]
  (update intr :interpreter/stmts conj v))

(defn end? "is at end"
  ^Boolean [{cur   :interpreter/current
             stmts :interpreter/stmts}]
  {:pre [(number? cur) (vector? stmts)]}
  (>= cur
      (dec (count stmts))))

(defn !end? "is not at end" ^Boolean [psr]
  (not (end? psr)))

(defn adv "advance" [intr]
  (cond-> intr
    (!end? intr) (update :interpreter/current inc)))

(defmulti expr-visitor (fn [type _] type))
(defmulti stmt-visitor (fn [type _] type))

(defn evaluate "evaluates expression" [expr]
  (.accept expr expr-visitor))

(defn execute "executes statement" [stmt]
  (.accept stmt stmt-visitor))

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

(defmethod expr-visitor :literal [_ expr]
  (.value expr))

(defmethod expr-visitor :grouping [_ expr]
  (evaluate (.expression expr)))

(defmethod expr-visitor :unary [_ expr]
  (let [right (evaluate (.right expr))
        op    (.operator expr)]
    (case (:token/kind op)
      :BANG  (not (boolean right))
      :MINUS (do
               (check-num-operand* op right)
               (- (double right)))
      nil)))

(defmethod expr-visitor :binary [_ expr]
  (let [left   (evaluate (.left expr))
        right  (evaluate (.right expr))
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
                 :else (.panic! (->RuntimeError op "Operands must be two numbers or two strings."))))
      nil)))

(defmethod stmt-visitor :expression [_ stmt]
  (evaluate (.expression stmt))
  nil)

(defmethod stmt-visitor :print
  [_ stmt]
  (println (evaluate (.expression stmt))))

(defmethod stmt-visitor :var
  [_ stmt]
  (let [v (some-> (.initializer stmt) evaluate)
        k (:token/lexeme (.name stmt))]
    {:env {:define {k v}}}))

(defn interpreter:new [& {stmts :statements}]
  {:interpreter/env            (env/env:new)
   :interpreter/runtime-error? false
   :interpreter/stmts          (or (some-> stmts not-empty vec) [])
   :interpreter/errors         []})

(defn interpret [intr & {stmts :statements}]
  (try
    (let [stmts (or (not-empty stmts)
                    (not-empty (:interpreter/stmts intr)))
          base  (dissoc intr :interpreter/stmts)
          exe   (fn [intr stmt]
                  (let [res  (execute stmt)
                        vars (some-> res :env
                                     :define
                                     not-empty
                                     first)]
                    (cond-> intr
                      vars  (update :interpreter/env env/-def (key vars) (val vars)))))]
      (reduce exe base stmts))
    (catch Exception e
      (println e)
      (-> intr
          (assoc :interpreter/runtime-error? true)
          (update :interpreter/errors conj e)))))
