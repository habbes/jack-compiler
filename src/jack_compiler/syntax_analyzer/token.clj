(ns jack-compiler.syntax-analyzer.token)

(defrecord Token [type value])

(defn is-type?
  [token t]
  (= (:type token) t))

(defn whitespace?
  [t]
  (is-type? t :whitespace))

(def not-whitespace?
  (complement whitespace?))

(defn is-value?
  [t v]
  (= (:value t) v))
