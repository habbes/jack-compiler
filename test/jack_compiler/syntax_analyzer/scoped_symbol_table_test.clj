(ns jack-compiler.syntax-analyzer.scoped-symbol-table-test
  (:require [clojure.test :refer :all]
            [jack-compiler.syntax-analyzer.scoped-symbol-table :refer :all]
            [jack-compiler.syntax-analyzer.symbol-table :as symt]))

(deftest init-table-test
  (testing "Creates record with class and subroutine scope tables"
    (let [tbl (init-table)]
      (is (= (.class tbl) (symt/symbol-table)))
      (is (= (.subroutine tbl) (symt/symbol-table))))))

(deftest add-entry-test
  (testing "Adds entry to class scope"
    (let [tbl (init-table)
          tbl (add-class-entry tbl {:name "x" :type "int" :kind :field })]
      (is (= (-> tbl .class .entries)
             {"x" (symt/->TableEntry "x" "int" :field 0)}))
      (testing "and increments segment counter"
        (is (= (-> tbl .class .counts :field) 1)))))
  (testing "Adds entry to subroutine scope"
    (let [tbl (init-table)
          tbl (add-subroutine-entry tbl {:name "y" :type "String" :kind :local})]
      (is (= (-> tbl .subroutine .entries)
             {"y" (symt/->TableEntry "y" "String" :local 0)}))
      (testing "and increments segment counter"
        (is (= (-> tbl .subroutine .counts :local) 1))))))

(deftest var-count-test
  (testing "Counts segment vars in class scope"
    (let [tbl (init-table)
          tbl (add-class-entry tbl {:name "bar" :type "Foo" :kind :field})
          tbl (add-class-entry tbl {:name "baz" :type "String" :kind :static})
          tbl (add-class-entry tbl {:name "x" :type "int" :kind :field})]
      (is (= (class-var-count tbl :field) 2))
      (is (= (class-var-count tbl :static) 1))))
  (testing "Counts segment vars in subroutine scope"
    (let [tbl (init-table)
          tbl (add-subroutine-entry tbl {:name "size" :type "int" :kind :arg})
          tbl (add-subroutine-entry tbl {:name "y" :type "String" :kind :local})
          tbl (add-subroutine-entry tbl {:name "obj" :type "MyClass" :kind :local})]
      (is (= (sub-var-count tbl :arg) 1))
      (is (= (sub-var-count tbl :local) 2)))))
