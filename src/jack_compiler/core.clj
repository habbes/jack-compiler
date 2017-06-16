(ns jack-compiler.core
  (:require [jack-compiler.syntax-analyzer.core :as sa]
            [jack-compiler.syntax-analyzer.xml-writer :refer [xml-writer]])
  (:gen-class))

(defn test-lexer
  [dir]
  (println "Lexing" dir)
  (let [h (xml-writer)]
    (sa/lex-and-handle-dir h dir))
  (println "Done."))

(defn -main
  "I don't do a whole lot ... yet."
  [dir]
  (test-lexer dir))
