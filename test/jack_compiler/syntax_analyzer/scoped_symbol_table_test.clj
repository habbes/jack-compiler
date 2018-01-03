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


(deftest kind-of-test
  (testing "When variable is in subroutine scope"
    (let [tbl (init-table)
          tbl (add-class-entry tbl {:name "x" :type "int" :kind :field})
          tbl (add-subroutine-entry tbl {:name "x" :type "String" :kind :local})]
      (is (= (kind-of tbl "x") :local))))
  (testing "When variable is un class but not subroutine scope"
    (let [tbl (init-table)
          tbl (add-class-entry tbl {:name "x" :type "int" :kind :field})]
      (is (= (kind-of tbl "x") :field))))
  (testing "When variable is not in symbol table"
    (let [tbl (init-table)]
      (is (= (kind-of tbl "x") nil)))))

(deftest attr-of-test
  (let [tbl (-> (init-table)
                (add-class-entry {:name "x" :type "int" :kind :field})
                (add-class-entry {:name "y" :type "String" :kind :static})
                (add-class-entry {:name "z" :type "MyClass" :kind :field})
                (add-subroutine-entry {:name "x" :type "bool" :kind :arg})
                (add-subroutine-entry {:name "w" :type "int" :kind :local})
                (add-subroutine-entry {:name "v" :type "String" :kind :arg}))
        fns [kind-of type-of index-of]]
    (testing "kind-of"
      (testing "Return subroutine var kind when in subroutine scope"
        (is (= (kind-of tbl "x") :arg))
        (is (= (kind-of tbl "w") :local)))
      (testing "Return class var kind when in class scope but not subroutine"
        (is (= (kind-of tbl "y") :static)))
      (testing "Returns nil if var is not in scope"
        (is (= (kind-of tbl "var") nil))))
    (testing "type-of"
      (testing "Returns subroutine var type when in subroutine scope"
        (is (= (type-of tbl "x") "bool"))
        (is (= (type-of tbl "w") "int")))
      (testing "Return  class var type when in class scope but not subroutine"
        (is (= (type-of tbl "y") "String")))
      (testing "Returns nil if var is not in scope"
        (is (= (type-of tbl "var") nil))))
    (testing "index-of"
      (testing "Returns subroutine var index in subroutine scope"
        (is (= (index-of tbl "x") 0))
        (is (= (index-of tbl "v") 1)))
      (testing "Return class var index in class scope but not subroutine"
        (is (= (index-of tbl "y") 0)))
      (testing "Returns nil if var is not in scope"
        (is (= (index-of tbl "var") nil))))))
