(ns jack-compiler.syntax-analyzer.parser-test
  (:require [clojure.test :refer :all]
            [jack-compiler.syntax-analyzer.parser :refer :all]
            [jack-compiler.syntax-analyzer.token :as tk]
            [jack-compiler.syntax-analyzer.parse-tree :as pt]))

; shortcut for token constructor
(def tkc tk/->Token)
; shortcut for ParseTree constructor
(def ptc pt/parse-tree)

(deftest consume-token-test
  (testing "Returns first token from seq and remaining seq"
    (let [ts [(tkc :keyword "int") (tkc :identifier "x")]
          [t ts-rest] (consume-token ts)]
      (is (= t (tkc :keyword "int")))
      (is (= ts-rest [(tkc :identifier "x")]))))
  (testing "Throws exception if no token found"
    (is (thrown-with-msg? Exception
                          #"unexpected end of stream"
                          (consume-token [])))))

(deftest consume-terminal-test
  (let [ts [(tkc :keyword "int") (tkc :identifier "x")]]
    (testing "Returns ParseTree node from matched token and remaining sq"
      (let [[p ts-rest] (consume-terminal ts :keyword)]
        (is (= p (ptc :keyword nil "int")))
        (is (= ts-rest [(tkc :identifier "x")]))))
    (testing "Throws exception if unexpected type"
      (is (thrown-with-msg? Exception
                            #"symbol expected but found keyword int"
                            (consume-terminal ts :symbol))))
    (testing "Returns ParseTree node from matched token and values and remaining sq"
      (let [[p ts-rest] (consume-terminal ts :keyword ["int" "boolean"])]
        (is (= p (ptc :keyword nil "int")))
        (is (= ts-rest [(tkc :identifier "x")]))))
    (testing "Throws exception if unexpected value"
      (is (thrown-with-msg? Exception
                            #"method or function expected but found int"
                            (consume-terminal ts :keyword ["method" "function"]))))))

(deftest consume-until-test
  ;; test consuming a sequence of commas with semicolon as the stop symbol
  (let [f #(consume-symbol % [","])
        stop-f (comp #(tk/is-value? % ";") first)]
    (testing "Consumes sequence of nodes based on parser fn until stop fn passes"
      (let [ts [(tkc :symbol ",") (tkc :symbol ",") (tkc :symbol ";")]
            [nodes ts] (consume-until ts stop-f f)]
        (is (= nodes [(ptc :symbol nil ",") (ptc :symbol nil ",")]))
        (is (= ts [(tkc :symbol ";")]))))
    (testing "Returns empty vec if stop-fn passes immediately"
      (let [ts [(tkc :symbol ";")]
            [nodes ts] (consume-until ts stop-f f)]
        (is (= nodes [])
            (= ts [(tkc :symbol ";")]))))))

(deftest consume-type-test
  (testing "Consumes int, boolean or char keyword into ParseTree"
    (doseq [k ["int" "char" "boolean"]]
      (let [ts [(tkc :keyword k)]
            [p ts-rest] (consume-type ts)]
        (is (= p (ptc :keyword nil k))))))
  (testing "Consumes identifier"
    (let [ts [(tkc :identifier "MyClass")]
          [p ts-rest] (consume-type ts)]
      (is (= p (ptc :identifier nil "MyClass")))))
  (testing "Throws exception if neither type keyword nor identifier"
    (is (thrown-with-msg? Exception
                          #"int or char or boolean expected but found let"
                          (consume-type [(tkc :keyword "let")])))
    (is (thrown-with-msg? Exception
                          #"identifier expected but found symbol"
                          (consume-type [(tkc :symbol "{")])))))

(deftest consume-op-test
  (testing "Consumes +,-,*,/,&,|,<,>,= into ParseTree"
    (doseq [op ["+" "-" "*" "-" "*" "/" "&" "|" "<" ">" "="]]
      (let [ts [(tkc :symbol op)]
            [p ts-rest] (consume-op ts)]
        (is (= p (ptc :symbol nil op))))))
  (testing "Throws exception if incorrect token found"
    (is (thrown-with-msg? Exception
                          #"symbol expected but found keyword let"
                          (consume-op [(tkc :keyword "let")])))
    (is (thrown-with-msg? Exception
                          #"expected but found \{"
                          (consume-op [(tkc :symbol "{")])))))

(deftest consume-unary-op-test
  (testing "Consumes - or ~ into ParseTree"
    (doseq [op ["-" "~"]]
      (let [ts [(tkc :symbol op)]
            [p ts-rest] (consume-unary-op ts)]
        (is (= p (ptc :symbol nil op))))))
  (testing "Throws exception if incorrect token found"
    (is (thrown-with-msg? Exception
                          #"symbol expected but found keyword let"
                          (consume-unary-op [(tkc :keyword "let")])))
    (is (thrown-with-msg? Exception
                          #"- or ~ expected but found +"
                          (consume-unary-op [(tkc :symbol "+")])))))

(deftest consume-keyword-constant-test
  (testing "Consumes true, false, null or this into ParseTree"
    (doseq [k ["true" "false" "null" "this"]]
      (let [ts [(tkc :keyword k)]
            [p ts-rest] (consume-keyword-constant ts)]
        (is (= p (ptc :keyword nil k))))))
  (testing "Throws exception if incorrect token found"
    (is (thrown-with-msg? Exception
                          #"true or false or null or this expected but found let"
                          (consume-keyword-constant [(tkc :keyword "let")])))
    (is (thrown-with-msg? Exception
                          #"keyword expected but found identifier x"
                          (consume-keyword-constant [(tkc :identifier "x")])))))

(deftest consume-comma-var-seq-test
  (testing "Consumes a seq of comma and var name until a semicolon is found"
    (let [ts [(tkc :symbol ",") (tkc :identifier "x")
              (tkc :symbol ",") (tkc :identifier "y")
              (tkc :symbol ";")]
          [ps ts-rest] (consume-comma-var-seq ts)]
      (is (= ps [(ptc :symbol nil ",") (ptc :identifier nil "x")
                 (ptc :symbol nil ",") (ptc :identifier nil "y")]))
      (is (= ts-rest [(tkc :symbol ";")]))))
  (testing "Returns empty vec if semicolon found immediately"
    (let [ts [(tkc :symbol ";")]
          [ps ts-rest] (consume-comma-var-seq ts)]
      (is (= ps []))
      (is (= ts-rest [(tkc :symbol ";")]))))
  (testing "Throws exception when wrong tokens found"
    (is (thrown-with-msg? Exception
                          #"identifier expected but found symbol"
                          (consume-comma-var-seq [(tkc :symbol ",") (tkc :symbol ";")])))
    (is (thrown-with-msg? Exception
                          #"symbol expected but found identifier x"
                          (consume-comma-var-seq [(tkc :identifier "x")])))))

(deftest consume-var-seq-test
  (testing "Consumes `varName(,varName)*;`"
    (testing "with one variable name"
      (let [ts [(tkc :identifier "x") (tkc :symbol ";")]
            [ps ts-rest] (consume-var-seq ts)]
        (is (= ps [(ptc :identifier nil "x") (ptc :symbol nil ";")]))))
    (testing "with many variable names"
      (let [ts [(tkc :identifier "x") (tkc :symbol ",")
                (tkc :identifier "y") (tkc :symbol ",")
                (tkc :identifier "z") (tkc :symbol ";")]
            [ps ts-rest] (consume-var-seq ts)]
        (is (= ps [(ptc :identifier nil "x") (ptc :symbol nil ",")
                   (ptc :identifier nil "y") (ptc :symbol nil ",")
                   (ptc :identifier nil "z") (ptc :symbol nil ";")])))))
  (testing "Throws exception when semicolon missing"
    (is (thrown-with-msg? Exception
                          #"unexpected end of stream"
                          (consume-var-seq [(tkc :identifier "x")])))
    (is (thrown-with-msg? Exception
                          #"unexpected end of stream"
                          (consume-var-seq [(tkc :identifier "x")
                                            (tkc :symbol ",")]))))
  (testing "Throws exception when wrong tokens found"
    (is (thrown-with-msg? Exception
                          #"identifier expected but found symbol"
                          (consume-var-seq [(tkc :symbol ",")])))
    (is (thrown-with-msg? Exception
                          #"identifier expected but found symbol"
                          (consume-var-seq [(tkc :symbol ";")])))))

(deftest parse-class-var-dec-test
  (testing "Parses `field|static type varName(,varName)*;`"
    (testing "with field"
      (let [ts [(tkc :keyword "field") (tkc :keyword "int")
                (tkc :identifier "x") (tkc :symbol ",")
                (tkc :identifier "y") (tkc :symbol ";")]
            [p ts-rest] (parse-class-var-dec ts)]
        (is (= p (ptc :classVarDec
                      [(ptc :keyword nil "field") (ptc :keyword nil "int")
                       (ptc :identifier nil "x") (ptc :symbol nil ",")
                       (ptc :identifier nil "y") (ptc :symbol nil ";")]
                      nil)))))
    (testing "with static"
      (let [ts [(tkc :keyword "static") (tkc :identifier "MyClass")
                (tkc :identifier "x") (tkc :symbol ";")]
            [p ts-rest] (parse-class-var-dec ts)]
        (is (= p (ptc :classVarDec
                      [(ptc :keyword nil "static") (ptc :identifier nil "MyClass")
                       (ptc :identifier nil "x") (ptc :symbol nil ";")]
                      nil))))))
  (testing "Throws exception when wrong keyword used"
    (is (thrown-with-msg? Exception
                          #"field or static expected but found let"
                          (parse-class-var-dec [(tkc :keyword "let")]))))
  (testing "Throws exception when token sequence is wrong"
    ;; type missing
    (is (thrown-with-msg? Exception
                          #"identifier expected but found symbol"
                          (parse-class-var-dec [(tkc :keyword "field")
                                                (tkc :identifier "x")
                                                (tkc :symbol ";")])))
    ;; no variables
    (is (thrown-with-msg? Exception
                          #"unexpected end of stream"
                          (parse-class-var-dec [(tkc :keyword "field")
                                                (tkc :keyword "int")])))
    ;; no type or vars
    (is (thrown-with-msg? Exception
                          #"unexpected end of stream"
                          (parse-class-var-dec [(tkc :keyword "field")])))))

(deftest parse-var-dec-test
  (testing "Parses `'var' type varName(,varName)*;`"
    (testing "with multiple vars"
      (let [ts [(tkc :keyword "var") (tkc :keyword "int")
                (tkc :identifier "x") (tkc :symbol ",")
                (tkc :identifier "y") (tkc :symbol ";")]
            [p ts-rest] (parse-var-dec ts)]
        (is (= p (ptc :varDec
                      [(ptc :keyword nil "var") (ptc :keyword nil "int")
                       (ptc :identifier nil "x") (ptc :symbol nil ",")
                       (ptc :identifier nil "y") (ptc :symbol nil ";")])))))
    (testing "with one var"
      (let [ts [(tkc :keyword "var") (tkc :identifier "MyClass")
                (tkc :identifier "x") (tkc :symbol ";")]
            [p ts-rest] (parse-var-dec ts)]
        (is (= p (ptc :varDec
                      [(ptc :keyword nil "var") (ptc :identifier nil "MyClass")
                       (ptc :identifier nil "x") (ptc :symbol nil ";")]))))))
  (testing "Throws exception when wrong keyword used"
    (is (thrown-with-msg? Exception
                          #"var expected but found field"
                          (parse-var-dec [(tkc :keyword "field")]))))
  (testing "Throws exception when token sequence is wrong"
    ;; type missing
    (is (thrown-with-msg? Exception
                          #"identifier expected but found symbol"
                          (parse-var-dec [(tkc :keyword "var")
                                                (tkc :identifier "x")
                                                (tkc :symbol ";")])))
    ;; no variables
    (is (thrown-with-msg? Exception
                          #"unexpected end of stream"
                          (parse-var-dec [(tkc :keyword "var")
                                                (tkc :keyword "int")])))
    ;; no type or vars
    (is (thrown-with-msg? Exception
                          #"unexpected end of stream"
                          (parse-var-dec [(tkc :keyword "var")])))))

(deftest parse-parameter-list-test
  (testing "Parses `( type varName (',' type varName)* )?`"
    (testing "with no parameters"
      (let [ts [(tkc :symbol ")")]
            [p ts] (parse-parameter-list ts)]
        (is (= p (ptc :parameterList [] nil)))
        (is (= ts [(tkc :symbol ")")]))))
    (testing "with one parameter"
      (let [ts [(tkc :keyword "int") (tkc :identifier "x")
                (tkc :symbol ")")]
            [p ts] (parse-parameter-list ts)]
        (is (= p (ptc :parameterList
                      [(ptc :keyword nil "int") (ptc :identifier nil "x")]
                      nil)))
        (is (= ts [(tkc :symbol ")")]))))
    (testing "with multiple params"
      (let [ts [(tkc :keyword "int") (tkc :identifier "x")
                (tkc :symbol ",")
                (tkc :identifier "Square") (tkc :identifier "sq")
                (tkc :symbol ",")
                (tkc :keyword "boolean") (tkc :identifier "check")
                (tkc :symbol ")")]
            [p ts] (parse-parameter-list ts)]
        (is (= p (ptc :parameterList
                      [(ptc :keyword nil "int") (ptc :identifier nil "x")
                       (ptc :symbol nil ",")
                       (ptc :identifier nil "Square") (ptc :identifier nil "sq")
                       (ptc :symbol nil ",")
                       (ptc :keyword nil "boolean") (ptc :identifier nil "check")]
                      nil)))
        (is (= ts [(tkc :symbol ")")]))))))


(deftest parse-term-test
  (testing "Parses varName"
    (let [ts [(tkc :identifier "x") (tkc :symbol ",")]
          [p ts] (parse-term ts)]
      (is (= p (ptc :term
                    [(ptc :identifier nil "x")])))
      (is (= ts [(tkc :symbol ",")])))))

(deftest parse-expression-test
  (testing "Parses 'term'"
    (let [ts [(tkc :identifier "x") (tkc :symbol ";")]
          [p ts] (parse-expression ts)]
      (is (= p (ptc :expression
                    [(ptc :term
                          [(ptc :identifier nil "x")])])))
      (is (= ts [(tkc :symbol ";")]))))
  (testing "Parses 'term op term'"
    (let [ts [(tkc :identifier "x") (tkc :symbol "+")
              (tkc :identifier "y") (tkc :symbol ";")]
          [p ts] (parse-expression ts)]
      (is (= p
             (ptc :expression
                  [(ptc :term [(ptc :identifier nil "x")])
                   (ptc :symbol nil "+")
                   (ptc :term [(ptc :identifier nil "y")])])))
      (is (= ts [(tkc :symbol ";")]))))
  (testing "Parses 'term (op term)*'"
    (let [ts [(tkc :identifier "x") (tkc :symbol "+")
              (tkc :identifier "y") (tkc :symbol "*")
              (tkc :identifier "z") (tkc :symbol ";")]
          [p ts] (parse-expression ts)]
      (is (= p
             (ptc :expression
                  [(ptc :term [(ptc :identifier nil "x")])
                   (ptc :symbol nil "+")
                   (ptc :term [(ptc :identifier nil "y")])
                   (ptc :symbol nil "*")
                   (ptc :term [(ptc :identifier nil "z")])])))
      (is (= ts [(tkc :symbol ";")])))))

(deftest parse-expression-list-test
  (testing "Parses empty expression list if a closing bracket is reached first"
    (let [ts [(tkc :symbol ")")]
          [p ts] (parse-expression-list ts)]
      (is (= p (ptc :expressionList [])))
      (is (= ts [(tkc :symbol ")")]))))
  (testing "Parses single expression up to a closing bracket"
    (let [ts [(tkc :identifier "x") (tkc :symbol ")")]
          [p ts] (parse-expression-list ts)]
      (is (= p (ptc :expressionList
                    [(ptc :expression
                          [(ptc :term
                                [(ptc :identifier nil "x")])])])))
      (is (= ts [(tkc :symbol ")")]))))
  (testing "Parses comma-delimited expressions up to a closing bracket"
    (let [ts [(tkc :identifier "x") (tkc :symbol ",")
              (tkc :identifier "y") (tkc :symbol ",")
              (tkc :identifier "z") (tkc :symbol ")")]
          [p ts] (parse-expression-list ts)]
      (is (= p (ptc :expressionList
                    [(ptc :expression
                          [(ptc :term
                                [(ptc :identifier nil "x")])])
                     (ptc :symbol nil ",")
                     (ptc :expression
                          [(ptc :term
                                [(ptc :identifier nil "y")])])
                     (ptc :symbol nil ",")
                     (ptc :expression
                          [(ptc :term
                                [(ptc :identifier nil "z")])])])))
      (is (= ts [(tkc :symbol ")")]))))
  (testing "Throws error on invalid delimiter"
    (let [ts [(tkc :identifier "x") (tkc :symbol ";")]]
      (is (thrown-with-msg? Exception
                            #", expected but found ;"
                            (parse-expression-list ts)))))
  (testing "Throws error when delimiter skipped"
    (let [ts [(tkc :identifier "x") (tkc :identifier "y")]]
      (is (thrown-with-msg? Exception
                            #"symbol expected but found identifier y"
                            (parse-expression-list ts)))))
  (testing "Throws error when next token is not identifier"
    (let [ts [(tkc :keyword "let") (tkc :identifier "x")]]
      (is (thrown-with-msg? Exception
                            #"identifier expected but found keyword let"
                            (parse-expression-list ts)))))
  (testing "Throws error when stream ends before closing bracket"
    (let [ts [(tkc :identifier "x")]]
      (is (thrown-with-msg? Exception
                            #"unexpected end of stream"
                            (parse-expression-list ts))))))

(deftest consume-subroutine-call-test
  (testing "Consumes standalone function call"
    (let [ts [(tkc :identifier "func") (tkc :symbol "(")
              (tkc :identifier "x") (tkc :symbol ")")
              (tkc :symbol ";")]
          [p ts] (consume-subroutine-call ts)]
      (is (= p [(ptc :identifier nil "func")
                (ptc :symbol nil "(")
                (ptc :expressionList
                     [(ptc :expression
                           [(ptc :term
                                 [(ptc :identifier nil "x")])])])
                (ptc :symbol nil ")")]))
      (is (= ts [(tkc :symbol ";")]))))
  (testing "Consumes class/instance bound method call"
    (let [ts [(tkc :identifier "var") (tkc :symbol ".")
              (tkc :identifier "func") (tkc :symbol "(")
              (tkc :symbol ")") (tkc :symbol ";")]
          [p ts] (consume-subroutine-call ts)]
      (is (= p [(ptc :identifier nil "var")
                (ptc :symbol nil ".")
                (ptc :identifier nil "func")
                (ptc :symbol nil "(")
                (ptc :expressionList [])
                (ptc :symbol nil ")")]))
      (is (= ts [(tkc :symbol ";")])))))

(deftest parse-statement-test
  (testing "let statement"
    (testing "Parses simple variable assignment"
      (let [ts [(tkc :keyword "let") (tkc :identifier "x")
                (tkc :symbol "=") (tkc :identifier "y")
                (tkc :symbol ";") (tkc :keyword "do")]
            [p ts] (parse-statement ts)]
        (is (= p (ptc :letStatement
                      [(ptc :keyword nil "let")
                       (ptc :identifier nil "x")
                       (ptc :symbol nil "=")
                       (ptc :expression
                            [(ptc :term
                                  [(ptc :identifier nil "y")])])
                       (ptc :symbol nil ";")])))
        (is (= ts [(tkc :keyword "do")]))))
    (testing "Parses array index assignment"
      (let [ts [(tkc :keyword "let") (tkc :identifier "items")
                (tkc :symbol "[") (tkc :identifier "i") (tkc :symbol "]")
                (tkc :symbol "=") (tkc :identifier "y")
                (tkc :symbol ";") (tkc :keyword "do")]
            [p ts] (parse-statement ts)]
        (is (= p (ptc :letStatement
                      [(ptc :keyword nil "let")
                       (ptc :identifier nil "items")
                       (ptc :symbol nil "[")
                       (ptc :expression
                            [(ptc :term
                                  [(ptc :identifier nil "i")])])
                       (ptc :symbol nil "]")
                       (ptc :symbol nil "=")
                       (ptc :expression
                            [(ptc :term
                                  [(ptc :identifier nil "y")])])
                       (ptc :symbol nil ";")])))
        (is (= ts [(tkc :keyword "do")])))))
  (testing "if statement"
    (testing "Parses if only statement"
      (let [ts [(tkc :keyword "if") (tkc :symbol "(")
                (tkc :identifier "x") (tkc :symbol ")")
                (tkc :symbol "{")
                (tkc :keyword "let") (tkc :identifier "x")
                (tkc :symbol "=") (tkc :identifier "y")
                (tkc :symbol ";")
                (tkc :symbol "}")
                (tkc :keyword "do")]
            [p ts] (parse-statement ts)]
        (is (= p (ptc :ifStatement
                      [(ptc :keyword nil "if")
                       (ptc :symbol nil "(")
                       (ptc :expression
                            [(ptc :term
                                  [(ptc :identifier nil "x")])])
                       (ptc :symbol nil ")")
                       (ptc :symbol nil "{")
                       (ptc :statements
                            [(ptc :letStatement
                                  [(ptc :keyword nil "let")
                                   (ptc :identifier nil "x")
                                   (ptc :symbol nil "=")
                                   (ptc :expression
                                        [(ptc :term
                                              [(ptc :identifier nil "y")])])
                                   (ptc :symbol nil ";")])])
                       (ptc :symbol nil "}")])))
        (is (= ts [(tkc :keyword "do")]))))
    (testing "Parses if-else statement"
      (let [ts [(tkc :keyword "if") (tkc :symbol "(")
                (tkc :identifier "x") (tkc :symbol ")")
                (tkc :symbol "{")
                (tkc :keyword "let") (tkc :identifier "x")
                (tkc :symbol "=") (tkc :identifier "y")
                (tkc :symbol ";")
                (tkc :symbol "}")
                (tkc :keyword "else")
                (tkc :symbol "{")
                (tkc :keyword "let") (tkc :identifier "x")
                (tkc :symbol "=") (tkc :identifier "z")
                (tkc :symbol ";")
                (tkc :symbol "}")
                (tkc :keyword "do")]
            [p ts] (parse-statement ts)]
        (is (= p (ptc :ifStatement
                      [(ptc :keyword nil "if")
                       (ptc :symbol nil "(")
                       (ptc :expression
                            [(ptc :term
                                  [(ptc :identifier nil "x")])])
                       (ptc :symbol nil ")")
                       (ptc :symbol nil "{")
                       (ptc :statements
                            [(ptc :letStatement
                                  [(ptc :keyword nil "let")
                                   (ptc :identifier nil "x")
                                   (ptc :symbol nil "=")
                                   (ptc :expression
                                        [(ptc :term
                                              [(ptc :identifier nil "y")])])
                                   (ptc :symbol nil ";")])])
                       (ptc :symbol nil "}")
                       (ptc :keyword nil "else")
                       (ptc :symbol nil "{")
                       (ptc :statements
                            [(ptc :letStatement
                                  [(ptc :keyword nil "let")
                                   (ptc :identifier nil "x")
                                   (ptc :symbol nil "=")
                                   (ptc :expression
                                        [(ptc :term
                                              [(ptc :identifier nil "z")])])
                                   (ptc :symbol nil ";")])])
                       (ptc :symbol nil "}")])))
        (is (= ts [(tkc :keyword "do")])))))
  (testing "while statement"
    (testing "Parses while statement"
      (let [ts [(tkc :keyword "while") (tkc :symbol "(")
                (tkc :identifier "x") (tkc :symbol ")")
                (tkc :symbol "{")
                (tkc :keyword "let") (tkc :identifier "x")
                (tkc :symbol "=") (tkc :identifier "y")
                (tkc :symbol ";")
                (tkc :symbol "}")
                (tkc :keyword "let")]
            [p ts] (parse-statement ts)]
        (is (= p (ptc :whileStatement
                      [(ptc :keyword nil "while")
                       (ptc :symbol nil "(")
                       (ptc :expression
                            [(ptc :term
                                  [(ptc :identifier nil "x")])])
                       (ptc :symbol nil ")")
                       (ptc :symbol nil "{")
                       (ptc :statements
                            [(ptc :letStatement
                                  [(ptc :keyword nil "let")
                                   (ptc :identifier nil "x")
                                   (ptc :symbol nil "=")
                                   (ptc :expression
                                        [(ptc :term
                                              [(ptc :identifier nil "y")])])
                                   (ptc :symbol nil ";")])])
                       (ptc :symbol nil "}")])))
        (is (= ts [(tkc :keyword "let")])))))
  (testing "do statement"
    (testing "Parses unbound function call"
      (let [ts [(tkc :keyword "do") (tkc :identifier "func")
                (tkc :symbol "(") (tkc :symbol ")")
                (tkc :symbol ";") (tkc :symbol "}")]
            [p ts] (parse-statement ts)]
        (is (= p (ptc :doStatement
                      [(ptc :keyword nil "do")
                       (ptc :identifier nil "func")
                       (ptc :symbol nil "(")
                       (ptc :expressionList [])
                       (ptc :symbol nil ")")
                       (ptc :symbol nil ";")])))
        (is (= ts [(tkc :symbol "}")]))))
    (testing "Parses bound method call"
      (let [ts [(tkc :keyword "do") (tkc :identifier "var")
                (tkc :symbol ".") (tkc :identifier "func")
                (tkc :symbol "(") (tkc :identifier "x")
                (tkc :symbol ")") (tkc :symbol ";")
                (tkc :symbol "}")]
            [p ts] (parse-statement ts)]
        (is (= p (ptc :doStatement
                      [(ptc :keyword nil "do")
                       (ptc :identifier nil "var")
                       (ptc :symbol nil ".")
                       (ptc :identifier nil "func")
                       (ptc :symbol nil "(")
                       (ptc :expressionList
                            [(ptc :expression
                                  [(ptc :term
                                        [(ptc :identifier nil "x")])])])
                       (ptc :symbol nil ")")
                       (ptc :symbol nil ";")])))
        (is (= ts [(tkc :symbol "}")])))))
  (testing "return statement"
    (testing "Parses return without expression"
      (let [ts [(tkc :keyword "return") (tkc :symbol ";")
                (tkc :symbol "}")]
            [p ts] (parse-statement ts)]
        (is (= p (ptc :returnStatement
                      [(ptc :keyword nil "return")
                       (ptc :symbol nil";")])))
        (is (= ts [(tkc :symbol "}")]))))
    (testing "Parses return statement with expression"
      (let [ts [(tkc :keyword "return") (tkc :identifier "x")
                (tkc :symbol ";") (tkc :symbol "}")]
            [p ts] (parse-statement ts)]
        (is (= p (ptc :returnStatement
                      [(ptc :keyword nil "return")
                       (ptc :expression
                            [(ptc :term
                                  [(ptc :identifier nil "x")])])
                       (ptc :symbol nil ";")])))
        (is (= ts [(tkc :symbol "}")]))))))

(deftest parse-statements-test
  (testing "Parses 0 statements when '}' reached first"
    (let [ts [(tkc :symbol "}")]
          [p ts] (parse-statements ts)]
      (is (= p (ptc :statements
                    [])))
      (is (= ts [(tkc :symbol "}")]))))
  (testing "Parses 1 statement"
    (let [ts [(tkc :keyword "return") (tkc :symbol ";")
              (tkc :symbol "}")]
          [p ts] (parse-statements ts)]
      (is (= p (ptc :statements
                    [(ptc :returnStatement
                          [(ptc :keyword nil "return")
                           (ptc :symbol nil ";")])])))
      (is (= ts [(tkc :symbol "}")]))))
  (testing "Parses multiple statements up to a '}'"
    (let [ts [(tkc :keyword "do") (tkc :identifier "func")
              (tkc :symbol "(") (tkc :symbol ")")
              (tkc :symbol ";")
              (tkc :keyword "return") (tkc :symbol ";")
              (tkc :symbol "}")]
          [p ts] (parse-statements ts)]
      (is (= p (ptc :statements
                    [(ptc :doStatement
                          [(ptc :keyword nil "do")
                           (ptc :identifier nil "func")
                           (ptc :symbol nil "(")
                           (ptc :expressionList [])
                           (ptc :symbol nil ")")
                           (ptc :symbol nil ";")])
                     (ptc :returnStatement
                          [(ptc :keyword nil "return")
                           (ptc :symbol nil ";")])])))
      (is (= ts [(tkc :symbol "}")])))))

(deftest parse-subroutine-body-test
  (testing "Parses subroutine `'{'varDec* statements'}'"
    (let [ts [(tkc :symbol "{")
              (tkc :keyword "var") (tkc :keyword "int")
              (tkc :identifier "x") (tkc :symbol ",")
              (tkc :identifier "y") (tkc :symbol ";")
              (tkc :keyword "var") (tkc :identifier "Foo")
              (tkc :identifier "bar") (tkc :symbol ";")
              (tkc :keyword "do") (tkc :identifier "baz")
              (tkc :symbol "(") (tkc :identifier "x")
              (tkc :symbol ")") (tkc :symbol ";")
              (tkc :keyword "return") (tkc :symbol ";")
              (tkc :symbol "}") (tkc :keyword "function")]
          [p ts] (parse-subroutine-body ts)]
      (is (= p (ptc :subroutineBody
                    [(ptc :symbol nil "{")
                     (ptc :varDec
                          [(ptc :keyword nil "var")
                           (ptc :keyword nil "int")
                           (ptc :identifier nil "x")
                           (ptc :symbol nil ",")
                           (ptc :identifier nil "y")
                           (ptc :symbol nil ";")])
                     (ptc :varDec
                          [(ptc :keyword nil "var")
                           (ptc :identifier nil "Foo")
                           (ptc :identifier nil "bar")
                           (ptc :symbol nil ";")])
                     (ptc :statements
                          [(ptc :doStatement
                                [(ptc :keyword nil "do")
                                 (ptc :identifier nil "baz")
                                 (ptc :symbol nil "(")
                                 (ptc :expressionList
                                      [(ptc :expression
                                            [(ptc :term
                                                  [(ptc :identifier nil "x")])])])
                                 (ptc :symbol nil ")")
                                 (ptc :symbol nil ";")])
                           (ptc :returnStatement
                                [(ptc :keyword nil "return")
                                 (ptc :symbol nil ";")])])
                     (ptc :symbol nil "}")])))
      (is (= ts [(tkc :keyword "function")]))))
  (testing "Parses body without vars"
    (let [ts [(tkc :symbol "{")
              (tkc :keyword "return") (tkc :identifier "x")
              (tkc :symbol ";")
              (tkc :symbol "}") (tkc :symbol "}")]
          [p ts] (parse-subroutine-body ts)]
      (is (= p (ptc :subroutineBody
                    [(ptc :symbol nil "{")
                     (ptc :statements
                          [(ptc :returnStatement
                                [(ptc :keyword nil "return")
                                 (ptc :expression
                                      [(ptc :term
                                            [(ptc :identifier nil "x")])])
                                 (ptc :symbol nil ";")])])
                     (ptc :symbol nil "}")])))
      (is (= ts [(tkc :symbol "}")]))))
  (testing "Parses empty body"
    (let [ts [(tkc :symbol "{") (tkc :symbol "}")
              (tkc :symbol "}")]
          [p ts] (parse-subroutine-body ts)]
      (is (= p (ptc :subroutineBody
                    [(ptc :symbol nil "{")
                     (ptc :statements [])
                     (ptc :symbol nil "}")])))
      (is (= ts [(tkc :symbol "}")])))))

(deftest parse-subroutine-dec-test
  (testing "Parses constructor declaration"
    (let [ts [(tkc :keyword "constructor") (tkc :identifier "Foo")
              (tkc :identifier "new") (tkc :symbol "(")
              (tkc :keyword "int") (tkc :identifier "x")
              (tkc :symbol ")") (tkc :symbol "{")
              (tkc :keyword "return") (tkc :identifier "x")
              (tkc :symbol ";") (tkc :symbol "}")]
          [p ts] (parse-subroutine-dec ts)]
      (is (= p (ptc :subroutineDec
                    [(ptc :keyword nil "constructor")
                     (ptc :identifier nil "Foo")
                     (ptc :identifier nil "new")
                     (ptc :symbol nil "(")
                     (ptc :parameterList
                          [(ptc :keyword nil "int")
                           (ptc :identifier nil "x")])
                     (ptc :symbol nil ")")
                     (ptc :subroutineBody
                          [(ptc :symbol nil "{")
                           (ptc :statements
                                [(ptc :returnStatement
                                      [(ptc :keyword nil "return")
                                       (ptc :expression
                                            [(ptc :term
                                                  [(ptc :identifier nil "x")])])
                                       (ptc :symbol nil ";")])])
                           (ptc :symbol nil "}")])]))))))
