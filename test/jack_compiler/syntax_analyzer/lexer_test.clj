(ns jack-compiler.syntax-analyzer.lexer-test
  (:require [clojure.test :refer :all]
            [jack-compiler.syntax-analyzer.lexer :refer :all]
            [clojure.string :as s])
  (:import [jack_compiler.syntax_analyzer.lexer Token]))


(deftest extract-token-test
  (testing "Extracts token if rule matches source"
    (let [src "function foo () { return; }"
          rule [#"^(function)" :keyword]
          [next-src token] (extract-token src rule)]
      (is (= token (Token. :keyword "function")))
      (is (= next-src " foo () { return; }"))))
  (testing "Returns nil if rule does not match"
    (let [src "function foo () {return; }"
          rule [#"^(class)" :keyword]
          out (extract-token src rule)]
      (is (= nil out)))))
