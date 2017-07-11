(ns jack-compiler.syntax-analyzer.parser-test
  (:require [clojure.test :refer :all]
            [jack-compiler.syntax-analyzer.parser :refer :all]
            [jack-compiler.syntax-analyzer.token :as tk]))

(deftest consume-token-test
  (testing "Returns first token from seq and remaining seq"
    (let [tkfn tk/->Token
          ts [(tkfn :keyword "int") (tkfn :identifier "x")]
          [t ts-rest] (consume-token ts)]
      (is (= t (tkfn :keyword "int")))
      (is (= ts-rest [(tkfn :identifier "x")])))))
