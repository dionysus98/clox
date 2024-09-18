(ns clox.lexer
  (:require [clox.token :as t]
            [clojure.string :as str]))

(defn <src
  "returns the substring based on the start and current of the lexer"
  ^String [lex]
  (subs (:lexer/src lex)
        (:lexer/start lex)
        (:lexer/current lex)))

(defn adv "advance" [lex]
  (update lex :lexer/current inc))

(defn report!
  ([lex ^String msg]
   (report! lex (<src lex) msg))
  ([lex ^String where ^String msg]
   (println (str "[line " (:lexer/line lex) "] Error " where ": " msg))
   (assoc lex :lexer/had-error? true)))

(defn at-end? [lex]
  (>= (:lexer/current lex)
      (count (:lexer/src lex))))

(defn add-token
  ([lex token-kind] (add-token lex token-kind nil))
  ([{:lexer/keys [start current src line] :as lex} token-kind literal]
   (let [text  (subs src start current)]
     (update lex :lexer/tokens conj (t/token:new token-kind text literal line)))))

(defn match? [{:lexer/keys [src current] :as lex}
              expected]
  (cond
    (at-end? lex) false
    (not= (nth src current) expected) false
    :else true))

(defn pk "peek lexer src" [lex]
  (cond
    (at-end? lex) \0
    :else (nth (:lexer/src lex) (:lexer/current lex))))

(defn pk-next "peek lexer src next" [lex]
  (let [nxt (inc (:lexer/current lex))
        src (:lexer/src lex)]
    (if
     (>= nxt (count src)) \0
     (nth src nxt))))

(defn digit? [^Character c]
  (re-matches #"[0-9]" (str c)))

(defn alpha? [^Character c]
  (re-matches #"[a-zA-Z]" (str c)))

(defn alphanumeric? [^Character c]
  (or (alpha? c) (digit? c)))

(defn scan-ident [lex]
  (let [lex  (loop [lex lex]
               (let [nxt (pk lex)]
                 (if (and (not (at-end? lex))
                          (alphanumeric? nxt))
                   (recur (adv lex))
                   lex)))
        kind (some-> lex <src
                     str/upper-case
                     keyword
                     t/keywords)]
    (if (keyword? kind)
      (add-token lex kind)
      (add-token lex :IDENT))))

(defn scan-number [lex]
  (let [frwd (fn [lex]
               (loop [lex lex]
                 (let [nxt (pk lex)]
                   (if (and (not (at-end? lex))
                            (digit? nxt))
                     (recur (adv lex))
                     lex))))
        lex  (frwd lex)
        nxt  (pk lex)]
    (if (and (= nxt \.)
             (digit? (pk-next lex)))
      (let [lex (frwd (adv lex))]
        (add-token lex :NUMBER (parse-double (<src lex))))
      (add-token lex :NUMBER (parse-long (<src lex))))))

(defn scan-string [lex]
  (let [lex  (loop [lex lex]
               (let [nxt (pk lex)]
                 (if (and (not= nxt \")
                          (not (at-end? lex)))
                   (if (= pk \newline)
                     (recur (update (adv lex) :lexer/line inc))
                     (recur (adv lex)))
                   lex)))]
    (if (at-end? lex)
      (report! lex "Unterminated String.")
      (let [lex  (adv lex)
            strv (subs (:lexer/src lex)
                       (inc (:lexer/start lex))
                       (dec (:lexer/current lex)))]
        (add-token lex :STRING strv)))))

(defn scan-token [lex]
  (let [cur  (:lexer/current lex)
        src  (:lexer/src lex)
        lex  (adv lex)
        chr   (nth src cur)]
    (case chr
      \newline (update lex :lexer/line inc)
      \space lex
      \( (add-token lex :LEFT-PAREN)
      \) (add-token lex :RIGHT-PAREN)
      \{ (add-token lex :LEFT-BRACE)
      \} (add-token lex :RIGHT-BRACE)
      \, (add-token lex :COMMA)
      \. (add-token lex :DOT)
      \- (add-token lex :MINUS)
      \+ (add-token lex :PLUS)
      \; (add-token lex :SEMICOLON)
      \* (add-token lex :STAR)
      \! (if (match? lex \=)
           (add-token (adv lex) :BANG-EQUAL)
           (add-token lex :BANG))
      \= (if (match? lex \=)
           (add-token (adv lex) :EQUAL-EQUAL)
           (add-token lex :EQUAL))
      \< (if (match? lex \=)
           (add-token (adv lex) :LESS-EQUAL)
           (add-token lex :LESS))
      \> (if (match? lex \=)
           (add-token (adv lex) :GREATER-EQUAL)
           (add-token lex :GREATER))
      \/ (if (match? lex \/)
           (loop [lex lex]
             (if (and (not= (pk lex) \newline)
                      (not (at-end? lex)))
               (recur (adv lex))
               lex))
           (add-token lex :SLASH))
      \"  (scan-string lex)
      (cond
        (digit? chr) (scan-number lex)
        (alpha? chr) (scan-ident lex)
        :else (report! lex "Unexpected character.")))))

(defn lex "scans the tokens" [lexer]
  (let [run (fn scan-next [{:lexer/keys [current line]
                            :as           lex}]
              (if-not (at-end? lex)
                (-> lex
                    (assoc :lexer/start current)
                    scan-token
                    scan-next)
                (update lex :lexer/tokens conj (t/token:new :EOF "" nil line))))]
    (run lexer)))

(defn lexer:new [^String src]
  {:lexer/src        src
   :lexer/tokens     []
   :lexer/line       1
   :lexer/start      0
   :lexer/current    0
   :lexer/had-error? false})

(comment
  (lex (lexer:new "var * = (+ 5 8); // asasas
                                  asas"))

  (lex (lexer:new "(* -123 45.67)"))

  (loop [lex (lexer:new (slurp "examples/main.clox"))]
    (println (pr-str (pk lex)))
    (when-not (at-end? lex)
      (recur (update lex :lexer/current inc))))
  :rcf)
