(ns clox.resolver
  (:refer-clojure :exclude [resolve declare])
  (:import  [java.util Stack])
  (:require [clox.ast]
            [clox.error :refer [->ResolverError]]))

(defonce ^:private function-types #{:NONE :FUNCTION})

(defmulti visitor (fn [_resl v] (type v)))

(defn- accept [resl v]
  (.accept v resl visitor))

(defn resolve [resl stmts]
  (reduce accept resl stmts))

(defn begin-scope [resl]
  (update resl :resl/scopes conj {}))

(defn end-scope [resl]
  (update resl :resl/scopes pop))

(defn- update-last [vec-xs ufn & args]
  (let [xsc (count vec-xs)]
    (if (> xsc 0)
      (conj (pop vec-xs) (apply ufn (peek vec-xs) args))
      vec-xs)))

(defn- scope-ul "updates last/new item in the :resl/scopes stack"
  [resl ufn & args]
  (apply update resl :resl/scopes update-last ufn args))

(defn declare
  [{scopes :resl/scopes
    :as    resl}
   {lx  :token/lexeme
    :as tk}]
  (cond
    (empty? scopes) resl
    (contains? (peek scopes) lx)
    #_____  (.panic! (->ResolverError tk "Already a variable with this name in this scope."))
    :else (scope-ul resl assoc lx false)))

(defn define [resl tk]
  (if (empty? (:resl/scopes resl))
    resl
    (scope-ul resl assoc (:token/lexeme tk) true)))

(defn peek-for
  "peeks the `:resl/scopes` stack and gets item from the peeked value for the given key"
  [resl k]
  (some-> resl :resl/scopes peek (get k)))

(defmethod visitor
  clox.ast.Block
  [resl ^clox.ast.Block stmt]
  (-> resl
      begin-scope
      (resolve (.statements stmt))
      end-scope))

(defmethod visitor
  clox.ast.Var
  [resl ^clox.ast.Var stmt]
  (let [name (.name stmt)
        init (.initializer stmt)]
    (-> resl
        (declare name)
        (cond-> init (accept init))
        (define name))))

(defn- resolve-local [resl expr tk]
  (let [intr     (:resl/intr resl)
        intr-rl  (:intr/resolve intr)
        scopes   (:resl/scopes resl)
        scopesc  (count scopes)
        _ (println {:scopes scopes})
        resolved (reduce
                  (fn [acc scope]
                    (let [ok? (contains? scope (:token/lexeme tk))]
                      (cond-> acc
                        ok?  (update-in [:resl :resl/intr] intr-rl expr (- scopesc (inc (:idx acc))))
                        true (update :idx inc))))
                  {:resl resl
                   :idx  0}
                  (reverse scopes))]
    (:resl resolved)))

(defmethod visitor
  clox.ast.Variable
  [resl ^clox.ast.Variable expr]
  (if (false? (:token (peek-for resl (:token/lexeme (.name expr)))))
    (.panic! (->ResolverError (.name expr) "can't read local variable in it's own scope."))
    (resolve-local resl expr (.name expr))))

(defmethod visitor
  clox.ast.Assign
  [resl ^clox.ast.Assign expr]
  (-> (accept resl (.value expr))
      (resolve-local expr (.name expr))))

(defn resolve-function
  [resl ^clox.ast.Function fn-stmt fn-type]
  {:pre [(fn-type function-types)]}
  (let [enclosing   (:resl/current-function resl)
        resl-begin  (-> resl
                        (assoc :resl/current-function fn-type)
                        begin-scope)
        resl-params (reduce
                     (fn [resl-acc param]
                       (-> resl-acc
                           (declare param)
                           (define param)))
                     resl-begin
                     (.params fn-stmt))
        resl-body   (accept resl-params (.body fn-stmt))]
    (-> resl-body
        end-scope
        (assoc :resl/current-function enclosing))))

(defmethod visitor
  clox.ast.Function
  [resl ^clox.ast.Function stmt]
  (-> resl
      (declare (.name stmt))
      (define  (.name stmt))
      (resolve-function stmt (:FUNCTION function-types))))

(defmethod visitor
  clox.ast.Expression
  [resl ^clox.ast.Expression stmt]
  (accept resl (.expression stmt)))

(defmethod visitor
  clox.ast.If
  [resl ^clox.ast.If stmt]
  (-> resl
      (accept (.condition stmt))
      (accept (.then-branch stmt))
      (cond->
       (.else-branch stmt) (accept (.else-branch stmt)))))

(defmethod visitor
  clox.ast.Print
  [resl ^clox.ast.Print stmt]
  (accept resl (.expression stmt)))

(defmethod visitor
  clox.ast.Return
  [resl ^clox.ast.Return stmt]
  (when (= (:resl/current-function resl) (function-types :NONE))
    (.panic! (->ResolverError (.keyword stmt) "Can't return from top-level code.")))
  (cond-> resl
    (.value stmt) (accept (.value stmt))))

(defmethod visitor
  clox.ast.While
  [resl ^clox.ast.While stmt]
  (-> resl
      (accept (.condition stmt))
      (accept (.body stmt))))

(defmethod visitor
  clox.ast.Binary
  [resl ^clox.ast.Binary expr]
  (-> resl
      (accept (.left expr))
      (accept (.right expr))))

(defmethod visitor
  clox.ast.Call
  [resl ^clox.ast.Call expr]
  (reduce accept
          (accept resl (.callee expr))
          (.arguments expr)))

(defmethod visitor
  clox.ast.Grouping
  [resl ^clox.ast.Grouping expr]
  (accept resl (.expression expr)))

(defmethod visitor
  clox.ast.Literal
  [resl ^clox.ast.Literal _]
  resl)

(defmethod visitor
  clox.ast.Logical
  [resl ^clox.ast.Logical expr]
  (-> resl
      (accept (.left expr))
      (accept (.right expr))))

(defmethod visitor
  clox.ast.Unary
  [resl ^clox.ast.Unary expr]
  (accept resl (.right expr)))


(defn resl:new "returns a new resolver ds"
  [& {intr   :interpreter
      scopes :scopes}]
  {:resl/intr   intr
   :resl/current-function (function-types :NONE)
   :resl/scopes (or [] scopes)})


(comment
  (def ss (Stack.))


  (.push ss {})
  (.push ss {:a 8})
  (.push ss {:a 8 :b 88})

  (.peek ss)
  (update [8 9 9 9 9 1] -1 inc)

  peek

  :rcf)

