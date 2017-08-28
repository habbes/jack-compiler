(ns jack-compiler.syntax-analyzer.xml-writer-test
  (:require [clojure.test :refer :all]
            [jack-compiler.syntax-analyzer.xml-writer :refer :all]
            [jack-compiler.syntax-analyzer.lexer :as lx]
            [jack-compiler.syntax-analyzer.core :as sa]
            [jack-compiler.syntax-analyzer.lexed-source :refer [->LexedSource]]
            [jack-compiler.syntax-analyzer.parsed-source :refer [->ParsedSource]]
            [jack-compiler.syntax-analyzer.parse-tree :refer [parse-tree]]
            [clojure.string :as s]
            [clojure.java.io :as io]
            [me.raynes.fs :as fs]))


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

(def ptc parse-tree)

(def exprless-class
  (ptc
    :class
    [(ptc :keyword nil "class")
     (ptc :identifier nil "Foo")
     (ptc :symbol nil "{")
     (ptc :classVarDec
          [(ptc :keyword nil "static")
           (ptc :keyword nil "int")
           (ptc :identifier nil "x")
           (ptc :symbol nil ";")])
     (ptc :classVarDec
          [(ptc :keyword nil "field")
           (ptc :keyword nil "boolean")
           (ptc :identifier nil "check")
           (ptc :symbol nil ",")
           (ptc :identifier nil "empty")
           (ptc :symbol nil ";")])
     (ptc :subroutineDec
          [(ptc :keyword nil "constructor")
           (ptc :identifier nil "Foo")
           (ptc :identifier nil "new")
           (ptc :symbol nil "(")
           (ptc :parameterList [])
           (ptc :symbol nil ")")
           (ptc :subroutineBody
                [(ptc :symbol nil "{")
                 (ptc :statements
                      [(ptc :returnStatement
                            [(ptc :keyword nil "return")
                             (ptc :symbol nil ";")])])
                 (ptc :symbol nil "}")])])
     (ptc :subroutineDec
          [(ptc :keyword nil "method")
           (ptc :keyword nil "void")
           (ptc :identifier nil "func")
           (ptc :symbol nil "(")
           (ptc :parameterList [])
           (ptc :symbol nil ")")
           (ptc :subroutineBody
                [(ptc :symbol nil "{")
                 (ptc :statements
                      [(ptc :returnStatement
                            [(ptc :keyword nil "return")
                             (ptc :symbol nil ";")])])
                 (ptc :symbol nil "}")])])
     (ptc :symbol nil "}")]))

(defn remove-spaces
  "Removes spaces in the specified string"
  [input]
  (s/replace input #"\s" ""))

(defn delete-if-exists
  "Deletes path if it exists"
  [path]
  (if (fs/exists? path)
    (fs/delete path)))

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
          out-path "test/test_files/SampleIfBlockT_Gen.xml"
          ls (->LexedSource src-path ts)]
      (delete-if-exists out-path)
      (sa/handle-lexed-source xw ls)
      (is (= true (fs/exists? out-path)))
      (is (= test-output (slurp out-path))))))

(deftest write-tree-test
  (testing "Generates xml representation of parse tree"
    (let [tw (xml-writer)
          t exprless-class
          w (java.io.StringWriter.)
          expected (slurp
                     "test/test_files/SampleExpressionLessClass.xml")]
      (sa/write-tree tw t w)
      (is (= (.toString w)
             expected)))))

(deftest handle-parsed-source-test
  (testing "Writes parse tree to xml file based on source path"
    (let [xw (xml-writer)
          tr exprless-class
          src-path "test/test_files/SampleExpressionLessClass.jack"
          out-path "test/test_files/SampleExpressionLessClass_Gen.xml"
          sample-path "test/test_files/SampleExpressionLessClass.xml"
          ps (->ParsedSource src-path tr)]
      (delete-if-exists out-path)
      (sa/handle-parsed-source xw ps)
      (is (= true (fs/exists? out-path)))
      (is (= (slurp sample-path)
             (slurp out-path))))))
