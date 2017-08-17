(ns jack-compiler.syntax-analyzer.core
  (:require [jack-compiler.file :as file]
            [jack-compiler.syntax-analyzer.lexer :as lx]
            [jack-compiler.syntax-analyzer.parser :as psr]
            [jack-compiler.syntax-analyzer.lexed-source]
            [clojure.java.io :as io])
  (:import [jack_compiler.syntax_analyzer.lexed_source LexedSource]
           [jack_compiler.syntax_analyzer.parsed_source ParsedSource]))

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


(defprotocol TokenWriter
  (write-tokens [tw ts w] "write tokens from the seq ts to the writer w"))

(defprotocol LexedSourceHandler
  (handle-lexed-source
    [h ls]
    "Handles a LexedSource instance, presumably with side-effects"))

(defn handle-lexed-sources
  "Handles a seq of lexed sources using the specified handler"
  [h lss]
  (doseq [ls lss]
    (handle-lexed-source h ls)))

(defn lex-and-handle-dir
  "Lexes all jack files in the specified dir and handles
  them using the specified handler"
  [h dir]
  (let [lss (lex-dir dir)]
    (handle-lexed-sources h lss)))

(defprotocol LexedSourceTransform
  (transform-lexed-source
    [t ls]
    "Transforms a LexedSource instance into something else"))

(def map-lexed-sources
  "Maps each lexed source in lss to a transformed output
  based on the transform t"
  [t lss]
  (map (partial transform-lexed-source t) lss))

(def lex-and-transform-dir
  "Lexes all jack files in the specified dir and
  transforms them using the specified transform,
  mapping each source to a transformed output"
  [t dir]
  (let [lss (lex-dir dir)]
    (map-lexed-sources t lss)))

(deftype Parser []
  LexedSourceTransform
  (transform-lexed-source
    [_ {:keys [path tokens]}]
    (let [tree (psr/parse tokens)]
      (ParsedSource. path tree))))

(defn parser
  "Creates an instance of Parser"
  []
  (Parser.))

(def parse-dir
  "Parses all jack files in the specified dir
  and returns a list of parsed sources"
  [dir]
  (let [p (parser)]
    (lex-and-transform-dir p dir)))

(defprotocol ParseTreeWriter
  (write-tree
    [tw t w]
    "Writes the parse tree t to the writer w"))

(defprotocol ParsedSourceHandler
  (handle-parsed-source
    [h ps]
    "Handles a ParsedSource instance"))

