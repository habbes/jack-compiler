(ns jack-compiler.syntax-analyzer.scoped-symbol-table
  (:require [jack-compiler.syntax-analyzer.symbol-table :as symt]))

(defrecord ScopedSymbolTable [class subroutine])

(defn init-table
  "Creates empty scope tables"
  []
  (ScopedSymbolTable. 
    (symt/symbol-table) (symt/symbol-table)))

(defn start-subroutine
  "Resets the subroutine scope symbol table"
  [tbl]
  (assoc-in tbl [:subroutine] (symt/symbol-table)))

(defn add-entry
  "Adds variable entry to the table in the specified scope"
  [tbl entry scope]
  (update-in tbl [scope] symt/add-entry entry))

(defn add-class-entry
  "Adds class-scope variable entry"
  [tbl entry]
  (add-entry tbl entry :class))

(defn add-subroutine-entry
  "Adds subroutine-scope variable entry"
  [tbl entry]
  (add-entry tbl entry :subroutine))

(defn var-count
  "Counts the number of vars of the
  specified kind in the given scope"
  [tbl kind scope]
  (-> tbl scope (symt/var-count kind)))

(defn class-var-count
  "Counts the number of class-level
  vars of the specified kind"
  [tbl kind]
  (var-count tbl kind :class))

(defn sub-var-count
  "Counts the number of subroutine-level
  vars of the specified kind"
  [tbl kind]
  (var-count tbl kind :subroutine))

(defn attr-of
  "Gets the value of the specified
  attribute of the specfied variable
  in the current scope"
  [tbl name attr]
  (if-let [val (-> tbl :subroutine (symt/var-attr name attr))]
    val
    (symt/kind-of (.class tbl) name)))

(defn kind-of
  "Gets the kind of the specified
  variable, returns nil if not found
  in current scope"
  [tbl name]
  (attr-of tbl name :kind))

(defn type-of
  "Gets the type of the specified
  variable in the current scope"
  [tbl name]
  (attr-of tbl name :type))

(defn index-of
  "Gets the index of the specified variable
  in the current scope"
  [tbl name]
  (attr-of tbl name :index))