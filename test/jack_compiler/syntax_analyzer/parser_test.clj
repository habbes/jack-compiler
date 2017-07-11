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
    (testing "Returns ParseTree node from matched token and value and remaining sq"
      (let [[p ts-rest] (consume-terminal ts :keyword "int")]
        (is (= p (ptc :keyword nil "int")))
        (is (= ts-rest [(tkc :identifier "x")]))))
    (testing "Throws exception if unexpected value"
      (is (thrown-with-msg? Exception
                            #"class expected but found int"
                            (consume-terminal ts :keyword "class"))))))
