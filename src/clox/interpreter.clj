(ns clox.interpreter
  (:require [clox.env :as env]
            [clox.error :refer [->RuntimeError]]))

(defmulti visitor (fn [type _] type))

(defn evaluate "evaluates expression" [expr]
  (.accept expr visitor))

(defn execute "executes statement" [stmt]
  (.accept stmt visitor))

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

(defmethod visitor :literal [_ expr]
  (.value expr))

(defmethod visitor :grouping [_ expr]
  (evaluate (.expression expr)))

(defmethod visitor :unary [_ expr]
  (let [right (evaluate (.right expr))
        op    (.operator expr)]
    (case (:token/kind op)
      :BANG  (not (boolean right))
      :MINUS (do
               (check-num-operand* op right)
               (- (double right)))
      nil)))

(defmethod visitor :binary [_ expr]
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

(defmethod visitor :expression [_ stmt]
  (evaluate (.expression stmt)))

(defmethod visitor :print [_ stmt]
  (println (evaluate (.expression stmt))))

(defmethod visitor :var [_ stmt]
  (some-> (.initializer stmt) evaluate))

(defn interpreter:new [& {stmts :statements}]
  {:interpreter/env            (env/env:new)
   :interpreter/runtime-error? false
   :interpreter/stmts          (or (some-> stmts not-empty vec) [])
   :interpreter/errors         []})

(defn interpret [intr]
  (try
    (update intr :interpreter/stmts (partial mapv execute))
    (catch Exception e
      (println e)
      (-> intr
          (assoc :interpreter/runtime-error? true)
          (update :interpreter/errors conj e)))))
