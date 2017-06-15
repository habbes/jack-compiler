(ns jack-compiler.syntax-analyzer.xml-token-writer-test
  (:require [clojure.test :refer :all]
            [jack-compiler.syntax-analyzer.xml-token-writer :refer :all]
            [jack-compiler.syntax-analyzer.lexer :as lx]
            [jack-compiler.syntax-analyzer.token :as tk]
            [clojure.string :as s])
  (:import [jack_compiler.syntax_analyzer.token Token]))


(def test-src
"if (x < 0)
  {let state = \"negative\";}")

(def test-output
"<tokens>
  <keyword> if </keyword>
  <symbol> ( </symbol>
  <identifier> x </identifier>
  <symbol> &lt; </symbol>
  <integerConstant> 0 </integerConstant>
  <symbol> ) </symbol>
  <symbol> { </symbol>
  <keyword> let </keyword>
  <identifier> state </identifier>
  <symbol> = </symbol>
  <stringConstant> negative </stringConstant>
  <symbol> ; </symbol>
  <symbol> } </symbol>
</tokens>")

(deftest write-tokens-test
  (testing "Represents token seq as xml string"
    (let [tw (xml-writer)
          ts (lx/token-seq test-src)
          w (java.io.StringWriter.)
          expected test-output]
      (lx/write-tokens tw ts w)
      (is (= (.toString w) expected)))))