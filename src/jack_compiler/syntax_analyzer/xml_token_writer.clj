(ns jack-compiler.syntax-analyzer.xml-token-writer
  (:require [jack-compiler.syntax-analyzer.lexer :refer [TokenWriter]]
            [jack-compiler.syntax-analyzer.token :as tk]))


(defn convert-symbol
  "Converts symbol to corresponding xml entity"
  [v]
  (case v
    "<" "&lt;"
    ">" "&gt;"
    "\"" "&quot;"
    "&" "&amp;"
    v))

(defn token-to-xml-value
  "Converts token value to xml-escaped value"
  [{v :value :as t}]
  (if (tk/is-type? t :symbol)
    (convert-symbol v)
    v))

(defn token-to-xml-element
  "Returns a string representing the token
  as an xml element"
  [{t :type :as token}]
  (str "<" (name t) "> "
       (token-to-xml-value token)
       " </" (name t) ">"))

(deftype XmlWriter []
  TokenWriter
  (write-tokens
    [_ ts w]
    (.write w "<tokens>\n")
    (doseq [t ts]
      (.write w (str "  " (token-to-xml-element t) "\n")))
    (.write w "</tokens>")))

(defn xml-writer
  "Returns an instance of XmlWriter"
  []
  (XmlWriter.))


