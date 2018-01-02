(ns jack-compiler.syntax-analyzer.symbol-table)

(defrecord TableEntry [name type kind index])

(defrecord SymbolTable [entries counts])

(defn symbol-table
  "Creates an empty symbol table"
  []
  (SymbolTable. {} {:local 0 :arg 0 :field 0 :static 0}))

(defn var-count
  "Gets the number of variables of the given kind
  in the symbol table"
  [tbl kind]
  (kind (:counts tbl))

(defn inc-count
  "Increment the counter for the specified variable
  kind in the symbol table"
  [tbl kind]
  (update-in tbl [:counts kind] inc))

(defn add-entry
  "Adds variable entry to the symbol table"
  [tbl {name :name kind :kind type :type :as entry}]
  (let [index (var-count tbl kind)]
    (->
      tbl
      assoc-in [:entries name] (TableEntry. name type kind index)
      inc-count kind)))

(defn var-attr
  "Gets the value of the specified attribute
  of the specified variable in the symbol table"
  [tbl name attr]
  (-> tbl :entries name attr))

(defn kind-of
  "Gets the kind of the specified variable"
  [tbl name]
  (var-attr tbl name :kind))

(defn type-of
  "Gets the type of the specified variable"
  [tbl name]
  (var-attr tbl name :type))

(defn index-of
  "Gets the segment index of the specified variable"
  [tbl name]
  (var-attr tbl name :index))