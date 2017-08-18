(ns jack-compiler.syntax-analyzer.xml-writer
  (:require [jack-compiler.syntax-analyzer.lexer :as lx]
            [jack-compiler.syntax-analyzer.core
             :refer [LexedSourceHandler TokenWriter
                     ParsedSourceHandler ParseTreeWriter] :as sa]
            [jack-compiler.syntax-analyzer.token :as tk]
            [jack-compiler.file :as file]
            [clojure.java.io :as io]
            [clojure.string :as s]))

;; string used as unit of indentation
(def indent-unit " ")

(defn indent
  "Returns an identation of the
  specified number of spaces"
  [n]
  (apply str (repeat n indent-unit)))

(defn convert-symbol
  "Converts symbol to corresponding xml entity"
  [v]
  (case v
    "<" "&lt;"
    ">" "&gt;"
    "\"" "&quot;"
    "&" "&amp;"
    v))

(defn open-tag
  "Returns open xml t tag"
  [t]
  (str "<" (name t) ">"))

(defn close-tag
  "Returns closing xml t tag"
  [t]
  (str "</" (name t) ">"))

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
  (let [out-path (file/get-output-path-for-file in-path "T_Gen.xml")]
    (with-open [w (io/writer out-path)]
      (sa/write-tokens tw tokens w))))

(defn tree-to-xml-value
  "Converts tree value to xml escaped value"
  [{v :value n :name}]
  (if (= n :symbol)
    (convert-symbol v)
    v))

(declare tree-to-xml)

(defn tree-seq-to-xml
  "Converts a seq of parse trees to xml"
  [ts level]
  (s/join
    (map #(tree-to-xml % level) ts)))

(defn tree-to-xml
  "Returns a string representing the parse tree
  as an xml tree"
  ([tree]
   (tree-to-xml tree 0))
  ([{n :name ch :children v :value :as t} level]
   (str (open-tag n)
        (if (nil? ch)
          (str " " (tree-to-xml-value t) " ")
          (str "\n" (indent level)
               (tree-seq-to-xml ch (inc level))
               "\n" (indent level)))
        (close-tag n))))

(defn write-tree-to-file
  "Writes parse tree to an output xml file corresponding
  to the in-path."
  [tw in-path tree]
  (let [out-path (file/get-output-path-for-file in-path "_Gen.xml")]
    (with-open [w (io/writer out-path)]
      (sa/write-tree tw tree w))))

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
    (write-tokens-to-file this path tokens))
  ParseTreeWriter
  (write-tree
    [_ t w]
    (.write w (tree-to-xml t)))
  ParsedSourceHandler
  (handle-parsed-source
    [this {:keys [path tree]}]
    (write-tree-to-file this path tree)))

(defn xml-writer
  "Returns an instance of XmlWriter"
  []
  (XmlWriter.))
