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

(defn is-next-period?
  "Checks whether the next token is a period '.'"
  [ts]
  (is-next-value? ts "."))

(defn after-next
  "Applies f to the tokens after the direct next one
  and returns the result"
  [[_ & next-ts] f]
  (f next-ts))

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

(defn consume-when
  "Consumes from ts using the specified parser f
  only if (test-f ts) passes. Returns vector of
  parsed nodes and remaining tokens. If (test-f ts)
  fails, parsed nodes will be an empty vector."
  [ts test-f f]
  (if (test-f ts)
    (f ts)
    [[] ts]))

(defn consume-choice
  "Consumes from ts using f-1 if (choice-f ts) passes
  otherwise consumes using f-2."
  [ts choice-f f-1 f-2]
  (if (choice-f ts)
    (f-1 ts)
    (f-2 ts)))

(defn consume-choices
  "Takes a token seq and a set of clauses.
  Each clause takes the form:
  pred f
  where pred is a function which when applied to ts
  should return a boolean
  and f is a parser function which should extract
  a parse tree and remaining tokens from ts
  The function will return (f ts) for the first (pred ts)
  to return true"
  [ts clauses]
  (loop [[pred f & next-clauses] clauses]
    (if-not pred
      (throw-parser-error
        "no valid choice found")
      (if (pred ts)
        (f ts)
        (recur next-clauses)))))

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

(defn consume-integer-constant
  "Consumes an integer constant from ts"
  [ts]
  (consume-terminal ts :integerConstant))

(defn consume-string-constant
  "Consumes a string constant"
  [ts]
  (consume-terminal ts :stringConstant))

(defn consume-var-name
  "Consumes a var name"
  [ts]
  (consume-identifier ts))

(declare parse-expression)
(declare parse-expression-list)

(defn consume-function-call
  "Consumes `subroutineName'('expressionList')'"
  [ts]
  (let [[id ts] (consume-identifier ts)
        [op-br ts] (consume-symbol ts ["("])
        [exprs ts] (parse-expression-list ts)
        [cl-br ts] (consume-symbol ts [")"])]
    [(nodes-vec id op-br exprs cl-br) ts]))

(defn consume-method-call
  "Consumes `(className|varName)'.'functionCall`"
  [ts]
  (let [[id ts] (consume-identifier ts)
        [dot ts] (consume-symbol ts ["."])
        [func ts] (consume-function-call ts)]
    [(nodes-vec id dot func) ts]))

(defn consume-subroutine-call
  "Consumes a standalone method or function call"
  [ts]
  (consume-choice
    ts
    #(after-next % is-next-period?)
    consume-method-call
    consume-function-call))

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

(defn consume-comma-expression
  "Consumes `','expression`"
  [ts]
  (let [[comma ts] (consume-symbol ts [","])
        [ex ts] (parse-expression ts)]
    [(nodes-vec comma ex) ts]))

(defn consume-comma-expression-seq
  "Consumes `(','expression)*`
  until a closing paranthesis"
  [ts]
  (consume-until
    ts
    #(is-next-value? % ")")
    consume-comma-expression))

(defn consume-expression-seq
  "Consumes one or more expressions separated
  by comma"
  [ts]
  (let [[fst ts] (parse-expression ts)
        [others ts] (consume-comma-expression-seq ts)]
    [(nodes-vec fst others) ts]))

(defn consume-when-expression-seq
  "Consumes an expression seq if the
  next token is not a closing parenthesis"
  [ts]
  (consume-when
    ts
    (complement #(is-next-value? % ")"))
    consume-expression-seq))

(defn parse-expression-list
  "Parses `(expression (','expression)*)?`
  when the next token is not a ')'"
  [ts]
  (let [[exprs ts] (consume-when-expression-seq ts)]
    [(pt/parse-tree :expressionList exprs) ts]))

(defn consume-array-index
  "Consumes an `'['expression']'` combination"
  [ts]
  (let [[open-br ts] (consume-symbol ts ["["])
        [ex ts] (parse-expression ts)
        [close-br ts] (consume-symbol ts ["]"])]
    [(nodes-vec open-br ex close-br) ts]))

(defn consume-when-array-index
  "Consume an array index `[expr]`
  if the next token is '['"
  [ts]
  (consume-when
    ts
    #(is-next-value? % "[")
    consume-array-index))

(defn parse-let-statement
  "Consumes `'let' varName ('['expression'])?'='expression';'`"
  [ts]
  (let [[kw ts] (consume-keyword ts ["let"])
        [iden ts] (consume-identifier ts)
        [index ts] (consume-when-array-index ts)
        [assgn ts] (consume-symbol ts ["="])
        [ex ts] (parse-expression ts)
        [sc ts] (consume-symbol ts [";"])]
    [(pt/parse-tree
       :letStatement
       (nodes-vec kw iden index assgn ex sc))
     ts]))

(defn consume-condition
  "Consumes a `'('expression')'` as used
  in condition statements"
  [ts]
  (let [[op-br ts] (consume-symbol ts ["("])
        [ex ts] (parse-expression ts)
        [cl-br ts] (consume-symbol ts [")"])]
    [(nodes-vec op-br ex cl-br) ts]))

(declare parse-statements)

(defn consume-statement-block
  "Consumes `'{'statements'}'`"
  [ts]
  (let [[op-br ts] (consume-symbol ts ["{"])
        [stms ts] (parse-statements ts)
        [cl-br ts] (consume-symbol ts ["}"])]
    [(nodes-vec op-br stms cl-br) ts]))

(defn consume-else-block
  "Consumes `'else''{'statements'}'`"
  [ts]
  (let [[kw ts] (consume-keyword ts ["else"])
        [stms ts] (consume-statement-block ts)]
    [(nodes-vec kw stms) ts]))

(defn consume-when-else-block
  "Consumes an else block
  if the next token is 'else'"
  [ts]
  (consume-when
    ts
    #(is-next-value? % ["else"])
    consume-else-block))

(defn parse-if-statement
  "Parses `'if'condition statement-block else-block?`"
  [ts]
  (let [[kw ts] (consume-keyword ts ["if"])
        [con ts] (consume-condition ts)
        [stms ts] (consume-statement-block ts)
        [else ts] (consume-when-else-block ts)]
    [(pt/parse-tree
       :ifStatement
       (nodes-vec kw con stms else))
     ts]))

(defn parse-while-statement
  "Parses `'while''('expression')''{'statements'}'`"
  [ts]
  (let [[kw ts] (consume-keyword ts ["while"])
        [con ts] (consume-condition ts)
        [stms ts] (consume-statement-block ts)]
    [(pt/parse-tree
       :whileStatement
       (nodes-vec kw con stms))
     ts]))

(defn parse-do-statement
  "Parses `'do' subroutineCall`"
  [ts]
  (let [[kw ts] (consume-keyword ts ["do"])
        [subr ts] (consume-subroutine-call ts)
        [sc ts] (consume-symbol ts [";"])]
    [(pt/parse-tree
       :doStatement
       (nodes-vec kw subr sc))
     ts]))

(defn consume-when-expression
  "Consumes expression if next token
  is not a semicolon"
  [ts]
  (consume-when
    ts
    (complement #(is-next-value? % ";"))
    parse-expression))

(defn parse-return-statement
  "Parses `'return' expression?';'`"
  [ts]
  (let [[rt ts] (consume-keyword ts ["return"])
        [ex ts] (consume-when-expression ts)
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

(defn consume-statement-seq
  "Consumes 0 or more statements
  until a '}' is found"
  [ts]
  (consume-until
    ts
    #(is-next-value? % "}")
    parse-statement))

(defn parse-statements
  "Parses `statement*`"
  [ts]
  (let [[stms ts] (consume-statement-seq ts)]
    [(pt/parse-tree :statements stms) ts]))

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

(defn consume-type
  "Consumes next token from ts if it's type name keyword
  or an identifier"
  [ts]
  (if (is-next-type? ts :keyword)
    (consume-keyword ts ["int" "char" "boolean"])
    (consume-identifier ts)))

(defn consume-return-type
  "Consumes next token from ts if it's a
  subroutine routine return type"
  [ts]
  (if (is-next-value? ts "void")
    (consume-keyword ts ["void"])
    (consume-type ts)))

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

(defn consume-var-dec-seq
  "Consumes a sequence of 0 or more varDec"
  [ts]
  (consume-until
    ts
    (complement #(is-next-value? % "var"))
    parse-var-dec))

(defn consume-subroutine-type
  "Consumes keyword if it's constructor|function|method"
  [ts]
  (consume-keyword
    ts
    ["constructor" "function" "method"]))

(defn parse-subroutine-body
  "Parses `'{'varDec* statements'}'"
  [ts]
  (let [[op-br ts] (consume-symbol ts ["{"])
        [vars ts] (consume-var-dec-seq ts)
        [stms ts] (parse-statements ts)
        [cl-br ts] (consume-symbol ts ["}"])]
    [(pt/parse-tree
       :subroutineBody
       (nodes-vec op-br vars stms cl-br))
     ts]))

(defn parse-subroutine-dec
  "Parses a subroutine declaration"
  [ts]
  (let [[sub-typ ts] (consume-subroutine-type ts)
        [rtyp ts] (consume-return-type ts)
        [id ts] (consume-identifier ts)
        [op-br ts] (consume-symbol ts ["("])
        [params ts] (parse-parameter-list ts)
        [cl-br ts] (consume-symbol ts [")"])
        [body ts] (parse-subroutine-body ts)]
    [(pt/parse-tree
       :subroutineDec
       (nodes-vec sub-typ rtyp id op-br params cl-br body))
     ts]))

(defn consume-subroutine-dec-seq
  "Parses 0 or more subroutine decs up to a '}'"
  [ts]
  (consume-until
    ts
    #(is-next-value? % "}")
    parse-subroutine-dec))

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

(defn consume-class-var-dec-seq
  "Consumes 0 or more `classVarDec`s"
  [ts]
  (consume-until
    ts
    (complement
      #(is-next-value? % ["field" "static"]))
    parse-class-var-dec))

(defn parse-class
  "Parses a class node
  `'class' className'{'classVarDec* subroutineDec*'}'`"
  [ts]
  (let [[cls ts] (consume-keyword ts ["class"])
        [id ts] (consume-identifier ts)
        [op-br ts] (consume-symbol ts ["{"])
        [vars ts] (consume-class-var-dec-seq ts)
        [subrs ts] (consume-subroutine-dec-seq ts)
        [cl-br ts] (consume-symbol ts ["}"])]
    [(pt/parse-tree
       :class
       (nodes-vec cls id op-br vars subrs cl-br))
     ts]))

(defn parse
  "Parses a seq of tokens ts into a ParseTree
  representing a Jack class"
  [ts]
  (nth (parse-class ts) 0))
