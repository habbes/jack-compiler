(ns jack-compiler.syntax-analyzer.xml-writer-test
  (:require [clojure.test :refer :all]
            [jack-compiler.syntax-analyzer.xml-writer :refer :all]
            [jack-compiler.syntax-analyzer.lexer :as lx]
            [jack-compiler.syntax-analyzer.core :as sa]
            [jack-compiler.syntax-analyzer.lexed-source :refer [->LexedSource]]
            [clojure.string :as s]
            [clojure.java.io :as io]))


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
      (sa/write-tokens tw ts w)
      (is (= (.toString w) expected)))))

(deftest handle-lexed-source-test
  (testing "Writes tokens as xml to xml file based on source path"
    (let [xw (xml-writer)
          ts (lx/token-seq test-src)
          src-path "test/test_files/SampleIfBlock.jack"
          out-path "test/test_files/SampleIfBlock.xml"
          ls (->LexedSource src-path ts)]
      (sa/handle-lexed-source xw ls)
      (is (= true (.exists (io/file out-path))))
      (is (= test-output (slurp out-path))))))
