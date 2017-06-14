(ns jack-compiler.syntax-analyzer.lexer-test
  (:require [clojure.test :refer :all]
            [jack-compiler.syntax-analyzer.lexer :refer :all]
            [clojure.string :as s])
  (:import [jack_compiler.syntax_analyzer.lexer Token]))

(def test-src
"if (x < 0)
  {let state = \"negative\";}")

(deftest extract-token-with-rule-test
  (testing "Extracts token if rule matches source"
    (let [src "function foo () { return; }"
          rule [#"^(function)" :keyword]
          [next-src token] (extract-token-with-rule src rule)]
      (is (= token (Token. :keyword "function")))
      (is (= next-src " foo () { return; }"))))
  (testing "Returns nil if rule does not match"
    (let [src "function foo () {return; }"
          rule [#"^(class)" :keyword]
          out (extract-token-with-rule src rule)]
      (is (= nil out)))))

(deftest extract-token-with-rules-test
  (testing "Extract token from a string given a seq of rules"
    (let [src "function foo () { return; }"
          rules [[#"^(class)" :keyword]
                 [#"^(function)" :keyword]
                 [#"^(\d+)" :integerConstant]]
          [next-src token] (extract-token-with-rules src rules)]
      (is (= token (Token. :keyword "function")))
      (is (= next-src " foo () { return; }"))))
  (testing "Also works with maps"
    (let [src "function foo () { return; }"
          rules {#"^(class)" :keyword
                 #"^(function)" :keyword
                 #"^(\d+)" :integerConstant}
          [next-src token] (extract-token-with-rules src rules)]
      (is (= token (Token. :keyword "function")))
      (is (= next-src " foo () { return; }"))))
  (testing "Returns nil if no rule matches"
    (let [src "function foo () { return; }"
          rules {#"^(class)" :keyword
                 #"^(\d+)" :integerConstant}
          out (extract-token-with-rules src rules)]
      (is (= nil out)))))

(deftest extract-token-test
  (testing "Extract token from string based on Jack syntax"
    (let [[next-src t1] (extract-token test-src)
          [next-src t2] (extract-token next-src)
          [next-src t3] (extract-token next-src)
          [next-src t4] (extract-token next-src)
          [next-src t5] (extract-token next-src)
          [next-src t6] (extract-token next-src)
          [next-src t7] (extract-token next-src)
          [next-src t8] (extract-token next-src)]
      (is (= t1 (Token. :keyword "if")))
      (is (= t2 (Token. :whitespace " ")))
      (is (= t3 (Token. :symbol "(")))
      (is (= t4 (Token. :identifier "x")))
      (is (= t5 (Token. :whitespace " ")))
      (is (= t6 (Token. :symbol "<")))
      (is (= t7 (Token. :whitespace " ")))
      (is (= t8 (Token. :integerConstant "0")))
      (is (= next-src ")\n  {let state = \"negative\";}"))))
  (testing "Returns nil on invalid syntax"
    (let [out (extract-token "'")]
      (is (= out nil)))))

(deftest token-seq-test
  (testing "Generates seq of non-whitespace tokens from source"
    (let [ts (into [] (token-seq test-src))]
      (is (= ts [(Token. :keyword "if")
                 (Token. :symbol "(")
                 (Token. :identifier "x")
                 (Token. :symbol "<")
                 (Token. :integerConstant "0")
                 (Token. :symbol ")")
                 (Token. :symbol "{")
                 (Token. :keyword "let")
                 (Token. :identifier "state")
                 (Token. :symbol "=")
                 (Token. :stringConstant "negative")
                 (Token. :symbol ";")
                 (Token. :symbol "}")]))))
  (testing "Throws exception on syntax error"
    (let [src "let `test=1;"]
      (is (thrown-with-msg? Exception
                           #"Syntax error"
                           (into [] (token-seq src)))))))
