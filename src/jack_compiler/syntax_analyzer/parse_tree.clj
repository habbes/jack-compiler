(ns jack-compiler.syntax-analyzer.parse-tree)

(defrecord ParseTree [name children value])

(defn parse-tree
  "Creates a parse tree with the specified name, children
  and value. children defaults to an empty vec
  and value to nil"
  ([name children value]
   (ParseTree. name children value))
  ([name children]
   (ParseTree. name children nil))
  ([name]
   (ParseTree. name [] nil)))

(defn terminal?
  "Checks whether pt is a terminal node"
  [pt]
  (nil? (.children pt)))

(defn simple-print
  "Prints a simple linear representation of the parse tree.
  Useful for debugging"
  [pt]
  (if (terminal? pt)
    (print (str "'" (:value pt) "' "))
    (do
      (print (str (:name pt) "("))
      (doseq [c (:children pt)]
        (simple-print c))
      (print ") "))))
