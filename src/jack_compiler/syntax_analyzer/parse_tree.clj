(ns jack-compiler.syntax-analyzer.parse-tree)

(defrecord ParseTree [name children value])

(defn terminal?
  "Checks whether pt is a terminal node"
  [pt]
  (empty? (.children pt)))
