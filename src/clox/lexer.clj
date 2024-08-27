(ns clox.lexer
  (:require [clox.token :as t]))

(defn adv "advance" [lex]
  (update lex :lexer/current inc))

(defn report!
  ([lex ^String msg]
   (report! lex "" msg))
  ([lex ^String where ^String msg]
   (println (str "[line " (:lexer/line lex) "] Error" where ": " msg))
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

(defn <src
  "returns the substring based on the start and current of the lexer"
  ^String [lex]
  (subs (:lexer/src lex)
        (:lexer/start lex)
        (:lexer/current lex)))

(defn scan-ident [lex]
  (let [lex  (loop [lex lex]
               (let [nxt (pk lex)]
                 (if (and (not (at-end? lex))
                          (alphanumeric? nxt))
                   (recur (adv lex))
                   lex)))
        kind (some-> lex <src keyword t/keywords)]
    (if (keyword? kind)
      (add-token lex kind)
      (add-token lex :ident))))

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
        (add-token lex :number (parse-double (<src lex))))
      (add-token lex :number (parse-long (<src lex))))))

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
        (add-token lex :greater strv)))))

(defn scan-token [lex]
  (let [cur  (:lexer/current lex)
        src  (:lexer/src lex)
        lex  (adv lex)
        chr   (nth src cur)]
    (case chr
      \newline (update lex :lexer/line inc)
      \space lex
      \( (add-token lex :left-paren)
      \) (add-token lex :right-paren)
      \{ (add-token lex :left-brace)
      \} (add-token lex :right-brace)
      \, (add-token lex :comma)
      \. (add-token lex :dot)
      \- (add-token lex :minus)
      \+ (add-token lex :plus)
      \; (add-token lex :semicolon)
      \* (add-token lex :star)
      \! (if (match? lex \=)
           (add-token (adv lex) :bang-equal)
           (add-token lex :bang))
      \= (if (match? lex \=)
           (add-token (adv lex) :equal-equal)
           (add-token lex :equal))
      \< (if (match? lex \=)
           (add-token (adv lex) :less-equal)
           (add-token lex :less))
      \> (if (match? lex \=)
           (add-token (adv lex) :greater-equal)
           (add-token lex :greater))
      \/ (if (match? lex \/)
           (loop [lex lex]
             (if (and (not= (pk lex) \newline)
                      (not (at-end? lex)))
               (recur (adv lex))
               lex))
           (add-token lex :slash))
      \"  (scan-string lex)
      (cond
        (digit? chr) (scan-number lex)
        (alpha? chr) (scan-ident lex)
        :else (report! lex "Unexpected character.")))))

(defn scan-tokens [lexer]
  (let [run (fn scan-next [{:lexer/keys [current line]
                            :as           lex}]
              (if-not (at-end? lex)
                (-> lex
                    (assoc :lexer/start current)
                    scan-token
                    scan-next)
                (update lex :lexer/tokens conj (t/token:new :eof "" nil line))))]
    (run lexer)))

(defn lexer:new [^String src]
  {:lexer/src        src
   :lexer/tokens     []
   :lexer/line       1
   :lexer/start      0
   :lexer/current    0
   :lexer/had-error? false})

(comment
  (scan-tokens (lexer:new "var * = (+ 5 8); // asasas
                                  asas"))

  (loop [lex (lexer:new (slurp "examples/main.clox"))]
    (println (pr-str (pk lex)))
    (when-not (at-end? lex)
      (recur (update lex :lexer/current inc))))
  :rcf)

