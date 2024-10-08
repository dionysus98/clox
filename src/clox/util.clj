(ns clox.util
  (:require [clojure.pprint :as pprint]
            [clox.error :refer [->RuntimeError]]))

(defmacro undefined-var! [tk msg]
  `(.panic! (->RuntimeError ~tk (str "undefined variable '" ~msg "'."))))

(defmacro println->
  "like clojure.core/-> but prints the final output before returning."
  [& args]
  `(let [res# (-> ~@args)
         m#   ~(meta &form)]
     (printf (str "----| "
                  ~(name (ns-name *ns*))
                  " | "
                  (:line m#)
                  ":"
                  (:column m#)
                  " >\n\n"
                  (with-out-str (pprint/pprint res#))
                  "\n----end--->\n"))
     res#))

(defmacro println->>
  "like clojure.core/->> but prints the final output before returning."
  [& args]
  `(let [res# (->> ~@args)
         m#   ~(meta &form)]
     (printf (str "----| "
                  ~(name (ns-name *ns*))
                  " | "
                  (:line m#)
                  ":"
                  (:column m#)
                  " >\n\n"
                  (with-out-str (pprint/pprint res#))
                  "\n----end--->\n"))
     res#))