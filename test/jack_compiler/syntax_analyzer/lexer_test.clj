(ns jack-compiler.syntax-analyzer.lexer-test
  (:require [clojure.test :refer :all]
            [jack-compiler.syntax-analyzer.lexer :refer :all]
            [clojure.string :as s])
  (:import [jack_compiler.syntax_analyzer.lexer Token]))


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
