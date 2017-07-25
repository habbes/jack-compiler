(ns jack-compiler.syntax-analyzer.parser
  (:require [jack-compiler.syntax-analyzer.token :as tk]
            [jack-compiler.syntax-analyzer.parse-tree :as pt]
            [clojure.string :as s]))

(defn throw-parser-error
  ([]
   (throw-parser-error "Parser error"))
  ([msg]
   (throw (ex-info msg {}))))

(defn consume-token
  "Consumes the next token from the seq. Returns a vector
  of the consumed token and the remaining tokens.
  Throws exception if no token found"
  [[t & ts]]
  (if (nil? t)
    (throw-parser-error "unexpected end of stream")
    [t ts]))

(defn consume-terminal
  "Consumes a terminal node of the specified type from ts.
  If values is provided, the node must match both type and be one of values.
  Vector of the remaining tokens and the consumed node is returned
  on success, otherwise an exception is thrown."
  ([ts t-type]
   (let [[t rest-ts] (consume-token ts)]
     (if (tk/is-type? t t-type)
       [(pt/->ParseTree t-type nil (:value t)) rest-ts]
       (throw-parser-error
         (str  (name t-type) " expected but found " (name (:type t)) " "(:value t))))))
  ([ts t-type values]
   (let [[node rest-ts] (consume-terminal ts t-type)]
     (if (some #{(:value node)} values)
       [node rest-ts]
       (throw-parser-error
         (str  (s/join " or " values) " expected but found " (:value node)))))))

(defn consume-identifier
  "Consumes next token from ts if it's an identifier."
  [ts]
  (consume-terminal ts :identifier))

(defn consume-keyword
  "Consumes next token from ts if it's a keyword."
  ([ts]
   (consume-terminal ts :keyword))
  ([ts values]
   (consume-terminal ts :keyword values)))

(defn consume-symbol
  "Consumes next token from ts if it's a symbol."
  ([ts]
   (consume-terminal ts :symbol))
  ([ts values]
   (consume-terminal ts :symbol values)))

(defn consume-type
  "Consumes next token from ts if it's type name keyword
  or an identifier"
  [[t :as ts]]
  (if (tk/is-type? t :keyword)
    (consume-keyword ts ["int" "char" "boolean"])
    (consume-identifier ts)))

(defn consume-comma-var-seq
  "Consume 0 or more pairs of comma followed by a var name
  until a semicolon is found.
  Returns a seq of the consumed tokens and seq of remaining
  tokens."
  ([ts]
   (consume-comma-var-seq ts []))
  ([[t :as ts] vars]
    (if (tk/is-value? t ";")
      [vars ts]
      (let [[comma ts] (consume-symbol ts [","])
            [iden ts] (consume-identifier ts)]
        (recur ts (conj vars comma iden))))))

(defn consume-var-seq
  "Consumes a sequence of var names separated by a comma
  and ending in a semicolon"
  [ts]
  (let [[fst ts] (consume-identifier ts)
        [others ts] (consume-comma-var-seq ts)
        [sc ts] (consume-symbol ts [";"])]
    [(flatten [fst others sc]) ts]))

(defn parse-class-var-dec
  "Parses a classVarDec node"
  [ts]
  (let [[field ts] (consume-keyword ts ["field" "static"])
        [typ ts] (consume-type ts)
        [vars ts] (consume-var-seq ts)]
    (pt/->ParseTree :classVarDec ((comp vec flatten conj)
                                  field typ vars)
                    nil)))

(defn parse-subroutine-dec
  [ts]
  (prn "TODO parse subroutine-dec")
  [])

(defn parse-*
  "Parses 0 or more instances of a program structure node based
  on the specified parser f."
  [ts f])

(defn parse-class
  "Parses a class node"
  [ts]
  (let [[cls ts] (consume-keyword ts ["class"])
        [id ts] (consume-identifier ts)
        [open-br ts] (consume-symbol ts ["{"])
        [vars ts] (parse-* ts parse-class-var-dec)
        [subrs ts] (parse-* ts parse-subroutine-dec)
        [close-br ts] (consume-symbol ts ["}"])]
    (pt/->ParseTree :class ((comp vec flatten conj)
                            cls id open-br vars subrs close-br)
                    nil)))
