(ns jack-compiler.syntax-analyzer.parser
  (:require [jack-compiler.syntax-analyzer.token :as tk]
            [jack-compiler.syntax-analyzer.parse-tree :as pt]))

(defn throw-parser-error
  ([]
   (throw-parser-error "Parser error"))
  ([msg]
   (throw (ex-info msg))))

(defn consume-token
  "Consumes the next token from the seq. Returns a vector
  of the consumed token and the remaining tokens"
  [[t & ts]]
  [t ts])

(defn consume-terminal
  "Consumes a terminal node of the specified type from ts.
  If value is provided, the node must match both type and value.
  Vector of the remaining tokens and the consumed node is returned
  on success, otherwise an exception is thrown."
  ([ts t-type]
   (let [[t rest-ts] (consume-token ts)]
     (if (tk/is-type? t t-type)
       [(pt/->ParseTree t-type nil (:value t)) rest-ts]
       (throw-parse-error
         (str  (name t-type) " expected but found " (:type t) (:value t))))))
  ([ts t-type value]
   (let [[node rest-ts] (consume-keyword ts t-type)]
     (if (= (:value node))
       node
       (throw-parse-error
         (str value "expected but found " (:value node)))))))

(defn parse-class-var-dec
  [ts]
  (prn "TODO parse class-var-dec")
  [])

(defn parse-subroutine-dec
  [ts]
  (prn "TODO parse subroutine-dec")
  [])

(defn parse-*
  "Parses 0 or more instances of a program structure node based
  on the specified parser f."
  [ts f])



(defn parse-class
  [ts]
  (let [[cls ts] (consume-terminal ts :keyword "class")
        [id ts] (consume-terminal ts :identitier)
        [open-br ts] (consume-terminal ts :symbol "{")
        [vars ts] (parse-* parse-class-var-dec)
        [subrs ts] (parse-* parse-subroutine-dec)
        [close-br ts] (consume-terminal ts :symbol "}")]
    (pt/->ParseTree :class ((comp vec flatten conj)
                            cls id open-br vars subrs close-br)
                    nil)))
