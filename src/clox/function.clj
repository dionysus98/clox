(ns clox.function 
  (:require
   [clox.callable :refer [ILoxCallable]]
   [clox.env :as env]
   [clox.error :refer [ILoxError]]))

(deftype LoxFunction [declaration closure]
  ILoxCallable
  (arity [_] (count (.params declaration)))
  (call  [this intr args]
    (let [callee (:token/lexeme (.name declaration))
          env    (let [base  {:i   0
                            ;; had to re-define var here as well. 
                            ;; since this closure won't have access to the loxfunction var, just the scope before it.
                              :env (env/push closure callee this)}
                       >args (fn [acc param]
                               (-> acc
                                   (update :env env/push (:token/lexeme param) (nth args (:i acc)))
                                   (update :i inc)))
                       res   (reduce >args base (.params declaration))]
                   (:env res))]
      (try
        (let [exe  (:intr/execute intr)
              res  (exe (assoc intr :intr/env env) (.body declaration))
              nenv (:intr/env res)]
          {:callee (LoxFunction. declaration nenv)
           :expr   (:expr res)})
        (catch Exception e
          (let [data (ex-data e)]
            (case (:cause data)
              :return {:expr (:value data)
                       :env  (-> data :intr :intr/env)}
              (println (ex-message e))))))))
  Object
  (toString [_] (str "<fn " (-> declaration .name :token/lexeme) ">")))

(deftype Return [intr value]
  ILoxError
  (panic! [_this]
    (throw (ex-info "returnException" {:cause :return :intr intr :value value}))))