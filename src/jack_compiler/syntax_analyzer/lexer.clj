(ns jack-compiler.syntax-analyzer.lexer
  (:import [jack-compiler.syntax-analyzer.token Token]))


(def token-res
  {#"class" :keyword
   #"constructor" :keyword
   #"function" :keyword
   #"method" :keyword
   #"field" :keyword
   #"static" :keyword
   #"var" :keyword
   #"int" :keyword
   #"char" :keyword
   #"boolean" :keyword
   #"void" :keyword
   #"true" :keyword
   #"false" :keyword
   #"null" :keyword
   #"this" :keyword
   #"let" :keyword
   #"do" :keyword
   #"if" :keyword
   #"else" :keyword
   #"while" :keyword
   #"return" :keyword

   #"\{" :symbol
   #"\}" :symbol
   #"\(" :symbol
   #"\)" :symbol
   #"\[" :symbol
   #"\]" :symbol
   #"\." :symbol
   #"," :symbol
   #";" :symbol
   #"\+" :symbol
   #"\-" :symbol
   #"\*" :symbol
   #"/" :symbol
   #"&" :symbol
   #"\|" :symbol
   #"<" :symbol
   #">" :symbol
   #"=" :symbol
   #"~" :symbol

   #"(\d+)" :integerConstant
   #"\"((?!\").*)\"" :stringConstant

   #"([A-Za-z_][A-Za-z0-9_]*)" :identifier})

