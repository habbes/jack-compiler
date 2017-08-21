(ns jack-compiler.core
  (:require [jack-compiler.syntax-analyzer.core :as sa]
            [jack-compiler.syntax-analyzer.xml-writer :refer [xml-writer]])
  (:gen-class))

(defn test-lexer
  "Lexes .jack files in the specified dir
  and outputs their tokens in correspnding
  xml files"
  [dir]
  (println "Lexing" dir)
  (let [h (xml-writer)]
    (sa/lex-and-handle-dir h dir))
  (println "Done."))

(defn test-parser
  [dir]
  "Parses .jack files in the specified dir
  and outputs their parse trees in correspnding
  xml files"
  (println "Parsing" dir)
  (let [h (xml-writer)]
    (sa/parse-and-handle-dir h dir))
  (println "Done."))

(defn -main
  "I don't do a whole lot ... yet."
  [dir]
  (test-parser dir))
