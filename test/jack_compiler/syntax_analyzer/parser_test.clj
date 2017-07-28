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

(deftest consume-*-test
  ;; test consuming a sequence of commas with semicolon as the stop symbol
  (let [f #(consume-symbol % [","])
        stop-f (comp #(tk/is-value? % ";") first)]
    (testing "Consumes sequence of nodes based on parser fn until stop fn passes"
      (let [ts [(tkc :symbol ",") (tkc :symbol ",") (tkc :symbol ";")]
            [nodes ts] (consume-* ts f stop-f)]
        (is (= nodes [(ptc :symbol nil ",") (ptc :symbol nil ",")]))
        (is (= ts [(tkc :symbol ";")]))))
    (testing "Returns empty vec if stop-fn passes immediately"
      (let [ts [(tkc :symbol ";")]
            [nodes ts] (consume-* ts f stop-f)]
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
