(ns jack-compiler.syntax-analyzer.xml-writer
  (:require [jack-compiler.syntax-analyzer.lexer :refer [TokenWriter] :as lx]
            [jack-compiler.syntax-analyzer.core :refer [LexedSourceHandler]]
            [jack-compiler.syntax-analyzer.token :as tk]
            [jack-compiler.file :as file]
            [clojure.java.io :as io]))


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

(defn write-tokens-to-file
  "Writes tokens to an output xml file corresponding
  to the in-path."
  [tw in-path tokens]
  (let [out-path (file/get-output-path-for-file in-path ".xml")]
    (with-open [w (io/writer out-path)]
      (lx/write-tokens tw tokens w))))

(deftype XmlWriter []
  TokenWriter
  (write-tokens
    [_ ts w]
    (.write w "<tokens>\n")
    (doseq [t ts]
      (.write w (str "  " (token-to-xml-element t) "\n")))
    (.write w "</tokens>"))
  LexedSourceHandler
  (handle-lexed-source
    [this {:keys [path tokens]}]
    (write-tokens-to-file this path tokens)))

(defn xml-writer
  "Returns an instance of XmlWriter"
  []
  (XmlWriter.))
