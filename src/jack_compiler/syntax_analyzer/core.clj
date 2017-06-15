(ns jack-compiler.syntax-analyzer.core
  (:require [jack-compiler.file :as file]
            [jack-compiler.syntax-analyzer.lexer :as lx]
            [clojure.java.io :as io]))

(defrecord LexedSource [path tokens])

(defn lex-source
  "Lexes the specified file and returned a
  LexedSource with the path and token seq"
  [path]
  (let [s (slurp path)
        ts (lx/token-seq s)]
    (LexedSource. path ts)))

(defn lex-dir
  "Lexes all the jack files in the specified dir
  and returns a seq of LexedSources"
  [dir]
  (let [paths (file/get-jack-files dir)]
    (map lex-source paths)))

(defprotocol LexedSourceHandler
  (handle-lexed-source [h ls] "handles a LexedSource instance"))

(defn handle-lexed-sources
  "Handles a seq of lexed sources using the specified handler"
  [h lss]
  (doseq [ls lss]
    (handle-lexed-source h ls)))

(defn lex-and-handle-dir
  [dir]
  (let [lss (lex-dir dir)]
    (handle-lexed-sources lss)))
