(ns clox.resolver
  (:import [java.util Stack]))



(defmulti expr-visitor (fn [type _resl _expr] type))
(defmulti stmt-visitor (fn [type _resl _stmt] type))

#_(defmethod stmt-visitor :block
    [_ resl stmt]
    (let [stmts (.statements stmt)
          encl  (env/env:new :env/enclosing env)
          base  (assoc intr :intr/env encl)
          exe   (fn [intr stmt] (execute intr stmt))
          intrn (reduce exe base stmts)
          env   (:env/enclosing (:intr/env intrn))]
      (-> intrn
          (sync-env env)
          (stmts+ nil))))

(defprotocol IResolver
  (resolve [this type stmt]))

(defrecord Resolver [intr]
  IResolver)