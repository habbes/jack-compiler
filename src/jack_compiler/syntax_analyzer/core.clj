(ns jack-compiler.syntax-analyzer.core
  (:require [jack-compiler.file :as file]
            [jack-compiler.syntax-analyzer.lexer :as lx]
            [jack-compiler.syntax-analyzer.xml-token-writer :refer xml-writer]
            [jack-compiler.syntax-analyzer.core.lexed-source]
            [clojure.java.io :as io])
  (:import [jack_compiler.syntax_analyzer.lexed_source LexedSource]))

(defn lex-source
  "Lexes the specified file and returned a
  LexedSource with the path and token seq"
  [path]
  (let [s (slurp path)
        ts (lx/token-seq s)]
    (LexedSource. path ts))

(defn lex-dir
  "Lexes all the jack files in the specified dir
  and returns a seq of LexedSources"
  [dir]
  (let [paths (file/get-jack-files dir)]
    (map lex-source paths)))

