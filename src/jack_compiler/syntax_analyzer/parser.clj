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
  "Checks whether the next token in ts matches values.
  If values is a list, then it must match one of the values."
  [[{v :value}] values]
  (let [values (if (sequential? values) values [values])]
    (some #{v} values)))

(defn is-next-type?
  "Checks whether the next token in ts has the specified type"
  [[{t :type}] typ]
  (= t typ))

(defn is-next-op?
  "Checks whether the next token is a binary operator"
  [ts]
  (is-next-value?
    ts
    ["+" "-" "*" "/" "&" "|" "<" ">" "="]))

(defn consume-until
  "Consumes 0 or more instances of a program structure
  based on the consumer function f applied repeatedly to
  ts untl (stop-f ts) returns true. f should return a vector
  of the parsed nodes and the remaining tokens"
  ([ts stop-f f]
   (consume-until ts stop-f f []))
  ([ts stop-f f nodes]
   (if (stop-f ts)
     [nodes ts]
     (let [[new-nodes ts] (f ts)
           nodes (nodes-vec nodes new-nodes)]
       (recur ts stop-f f nodes)))))

(defn consume-if
  "Consumes from ts using the specified parser f
  only if (test-f ts) passes. Returns vector of
  parsed nodes and remaining tokens. If (test-f ts)
  fails, parsed nodes will be an empty vector."
  [ts test-f f]
  (if (test-f ts)
    (f ts)
    [[] ts]))

(defn consume-terminal
  "Consumes a terminal node of the specified type from ts.
  If values is provided, the node must match both type and be one of values.
  Vector of the remaining tokens and the consumed node is returned
  on success, otherwise an exception is thrown."
  ([ts t-type]
   (let [[t rest-ts] (consume-token ts)]
     (if (tk/is-type? t t-type)
       [(pt/parse-tree t-type nil (:value t)) rest-ts]
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

(defn consume-op
  "Consumes a binary operator node"
  [ts]
  (consume-symbol
    ts
    ["+" "-" "*" "/" "&" "|" "<" ">" "="]))

(defn consume-unary-op
  "Consumes a unary operator node"
  [ts]
  (consume-symbol ts ["-" "~"]))

(defn consume-keyword-constant
  "Consumes a keyword constant value"
  [ts]
  (consume-keyword
    ts
    ["true" "false" "null" "this"]))

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
  (consume-until
    ts
    #(is-next-value? % ";")
    consume-comma-var))

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
  (consume-until
    ts
    #(is-next-value? % ")")
    consume-comma-type-var))

(defn parse-class-var-dec
  "Parses a classVarDec node"
  [ts]
  (let [[field ts] (consume-keyword ts ["field" "static"])
        [typ ts] (consume-type ts)
        [vars ts] (consume-var-seq ts)]
    [(pt/parse-tree
       :classVarDec
       (nodes-vec field typ vars))
     ts]))

(defn parse-var-dec
  "Parses a varDec node `'var' type varName (',' varName)*';'`"
  [ts]
  (let [[kw ts] (consume-keyword ts ["var"])
        [typ ts] (consume-type ts)
        [vars ts] (consume-var-seq ts)]
    [(pt/parse-tree
       :varDec
       (nodes-vec kw typ vars))
     ts]))

(defn parse-parameter-list
  "Parses a subroutine parameter list"
  [ts]
  (if (is-next-value? ts ")")
    [(pt/parse-tree :parameterList) ts]
    (let [[fst ts] (consume-type-var ts)
          [others ts] (consume-comma-type-var-seq ts)]
      [(pt/parse-tree
         :parameterList
         (nodes-vec fst others))
       ts])))

(defn parse-term
  "Parses a term node"
  ;; TODO: simplified version that only parses var names
  [ts]
  (let [[iden ts] (consume-identifier ts)]
    [(pt/parse-tree :term [iden])
     ts]))

(defn consume-op-term
  "Consumes an `op term` combination"
  [ts]
  (let [[op ts] (consume-op ts)
        [term ts] (parse-term ts)]
    [(nodes-vec op term) ts]))

(defn consume-op-term-seq
  "Consumes a `(op term)*` sequence"
  [ts]
  (consume-until
    ts
    (complement is-next-op?)
    consume-op-term))

(defn parse-expression
  "Parses an expression"
  [ts]
  (let [[fst ts] (parse-term ts)
        [others ts] (consume-op-term-seq ts)]
    [(pt/parse-tree
       :expression
       (nodes-vec fst others))
     ts]))

(defn consume-array-index
  "Consumes an `'['expression']'` combination"
  [ts]
  (let [[open-br ts] (consume-symbol ts "[")
        [ex ts] (parse-expression ts)
        [close-br ts] (consume-symbol ts "]")]
    [(nodes-vec open-br ex close-br) ts]))

(defn consume-if-array-index
  "Consume an array index `[expr]`
  if the next token is '['"
  [ts]
  (consume-if
    ts
    #(is-next-value? % "[")
    consume-array-index))

(defn parse-let-statement
  "Consumes `'let' varName ('['expression'])?'='expression';'`"
  [ts]
  (let [[kw ts] (consume-keyword ts ["let"])
        [iden ts] (consume-identifier ts)
        [index ts] (consume-if-array-index ts)
        [assgn ts] (consume-symbol ts ["="])
        [ex ts] (consume-expression ts)
        [sc ts] (consume-symbol ts [";"])]
    [(pt/parse-tree
       :letStatement
       (nodes-vec kw iden index assgn ex sc))
     ts]))

(defn parse-if-statement
  [ts]
  "TODO: implement this")

(defn parse-while-statement
  [ts]
  "TODO: implement this")

(defn parse-do-statement
  [ts]
  "TODO: implement this")

(defn parse-return-statement
  "Parses `'return' expression?';'`"
  [ts]
  (let [[rt ts] (consume-keyword ts ["return"])
        [ex ts] (consume-if
                  ts
                  (not #(is-next-value? % [";"]))
                  parse-expression)
        [sc ts] (consume-symbol ts [";"])]
    [(pt/parse-tree
       :returnStatement
       (nodes-vec rt ex sc))
     ts]))

(defn parse-statement
  "Parses a statement"
  [[{v :value t :type} :as ts]]
  (let [f (case v
            "let" parse-let-statement
            "if" parse-if-statement
            "while" parse-while-statement
            "do" parse-do-statement
            "return" parse-return-statement
            (throw-parser-error
              (str "statement expected but found "
                   (name t) " " v)))]
    (f ts)))

(defn parse-statements
  "Parses `statement*`"
  [ts]
  (let [[stms ts] (consume-until
                    ts
                    #(is-next-value? % ["}"])
                    parse-statement)]
    [(pt/parse-tree :statements stms) ts]))

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
        [vars ts] (consume-until ts parse-class-var-dec)
        [subrs ts] (consume-until ts parse-subroutine-dec)
        [close-br ts] (consume-symbol ts ["}"])]
    [(pt/parse-tree
       :class
       (nodes-vec cls id open-br vars subrs close-br))
     ts]))
