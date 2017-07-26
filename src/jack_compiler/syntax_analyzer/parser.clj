(ns jack-compiler.syntax-analyzer.parser
  (:require [jack-compiler.syntax-analyzer.token :as tk]
            [jack-compiler.syntax-analyzer.parse-tree :as pt]
            [clojure.string :as s]))

(defn throw-parser-error
  "Throws exception with specifed message"
  ([]
   (throw-parser-error "Parser error"))
  ([msg]
   (throw (ex-info msg {}))))

(defn nodes-vec
  "Returns a linear un-nested vec of all arguments"
  [& nodes]
  (apply (partial (comp vec flatten conj) []) nodes))

(defn consume-token
  "Consumes the next token from the seq. Returns a vector
  of the consumed token and the remaining tokens.
  Throws exception if no token found"
  [[t & ts]]
  (if (nil? t)
    (throw-parser-error "unexpected end of stream")
    [t ts]))

(defn is-next-value?
  "Checks whether the next token has the specified value"
  [[{v :value}] value]
  (= v value))

(defn is-next-type?
  "Checks whether the next token in ts has the specified type"
  [[{t :type}] typ]
  (= t typ))

(defn consume-*
  "Consumes 0 or more instances of a program structure
  based on the consumer function f applied repeatedly to
  ts untl (stop-f ts) returns true. f should return a vector
  of the parsed nodes and the remaining tokens"
  ([ts f stop-f]
   (consume-* ts f stop-f []))
  ([ts f stop-f nodes]
   (if (stop-f ts)
     [nodes ts]
     (let [[new-nodes ts] (f ts)
           nodes (nodes-vec nodes new-nodes)]
       (recur ts f stop-f nodes)))))

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
  [ts]
  (if (is-next-type? ts :keyword)
    (consume-keyword ts ["int" "char" "boolean"])
    (consume-identifier ts)))

(defn consume-comma-var
  "Consumes a `',' varName` combination"
  [ts]
  (let [[comma ts] (consume-symbol ts [","])
        [iden ts] (consume-identifier ts)]
    [[comma iden] ts]))

(defn consume-comma-var-seq
  "Consume 0 or more pairs of comma followed by a var name
  until a semicolon is found.
  Returns a seq of the consumed tokens and seq of remaining
  tokens."
  [ts]
  (consume-* ts
             consume-comma-var
             #(is-next-value? % ";")))

(defn consume-var-seq
  "Consumes a sequence of var names separated by a comma
  and ending in a semicolon"
  [ts]
  (let [[fst ts] (consume-identifier ts)
        [others ts] (consume-comma-var-seq ts)
        [sc ts] (consume-symbol ts [";"])]
    [(nodes-vec fst others sc) ts]))

(defn consume-type-var
  "Consumes a `type varName` combination"
  [ts]
  (let [[typ ts] (consume-type ts)
        [iden ts] (consume-identifier ts)]
    [[typ iden] ts]))

(defn consume-comma-type-var
  "Consumes `',' type varName` combination"
  [ts]
  (let [[comma ts] (consume-symbol ts [","])
        [typ ts] (consume-type ts)
        [iden ts] (consume-identifier ts)]
    [[comma typ iden] ts]))

(defn consume-comma-type-var-seq
  "Consumes a `(',' type varName)*` sequence for
  for parameter list"
  [ts]
  (consume-* ts
             consume-comma-type-var*
             #(is-next-value? % ")")))

(defn parse-parameter-list
  "Parses a subroutine parameter list"
  [ts]
  (let [[open-br ts] (consume-symbol ts ["("])
        [fst ts] (consume-type-var ts)
        [others ts] (consume-comma-type-var-seq ts)
        [close-br ts] (consume-symbol ts [")"])]
    [(nodes-vec open-br fst others close-br) ts]))

(defn parse-class-var-dec
  "Parses a classVarDec node"
  [ts]
  (let [[field ts] (consume-keyword ts ["field" "static"])
        [typ ts] (consume-type ts)
        [vars ts] (consume-var-seq ts)]
    [(pt/->ParseTree :classVarDec
                     (nodes-vec field typ vars)
                     nil)
     ts]))

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
        [vars ts] (consume-* ts parse-class-var-dec)
        [subrs ts] (consume-* ts parse-subroutine-dec)
        [close-br ts] (consume-symbol ts ["}"])]
    [(pt/->ParseTree :class
                     (nodes-vec cls id open-br vars subrs close-br)
                     nil)
     ts]))
