(ns jack-compiler.syntax-analyzer.parser-test
  (:require [clojure.test :refer :all]
            [jack-compiler.syntax-analyzer.parser :refer :all]
            [jack-compiler.syntax-analyzer.token :as tk]
            [jack-compiler.syntax-analyzer.parse-tree :as pt]))

; shortcut for token constructor
(def tkc tk/->Token)
; shortcut for ParseTree constructor
(def ptc pt/->ParseTree)

(deftest consume-token-test
  (testing "Returns first token from seq and remaining seq"
    (let [tkfn tk/->Token
          ts [(tkfn :keyword "int") (tkfn :identifier "x")]
          [t ts-rest] (consume-token ts)]
      (is (= t (tkfn :keyword "int")))
      (is (= ts-rest [(tkfn :identifier "x")])))))

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
