(ns jack-compiler.syntax-analyzer.lexer
  (:require [clojure.string :as str]))
  ;(:import [jack_compiler.syntax_analyzer.token Token]))

(defrecord Token [type value])

(def token-rules
  {#"^(\s+)" :whitespace
   #"^(//\.*)" :whitespace
   ;TODO: add regex for multiline comment as whitespace

   #"^(class)" :keyword
   #"^(constructor)" :keyword
   #"^(function)" :keyword
   #"^(method)" :keyword
   #"^(field)" :keyword
   #"^(static)" :keyword
   #"^(var)" :keyword
   #"^(int)" :keyword
   #"^(char)" :keyword
   #"^(boolean)" :keyword
   #"^(void)" :keyword
   #"^(true)" :keyword
   #"^(false)" :keyword
   #"^(null)" :keyword
   #"^(this)" :keyword
   #"^(let)" :keyword
   #"^(do)" :keyword
   #"^(if)" :keyword
   #"^(else)" :keyword
   #"^(while)" :keyword
   #"^(return)" :keyword

   #"^(\{)" :symbol
   #"^(\})" :symbol
   #"^(\()" :symbol
   #"^(\))" :symbol
   #"^(\[)" :symbol
   #"^(\])" :symbol
   #"^(\.)" :symbol
   #"^(,)" :symbol
   #"^(;)" :symbol
   #"^(\+)" :symbol
   #"^(\-)" :symbol
   #"^(\*)" :symbol
   #"^(/)" :symbol
   #"^(&)" :symbol
   #"^(\|)" :symbol
   #"^(<)" :symbol
   #"^(>)" :symbol
   #"^(=)" :symbol
   #"^(~)" :symbol

   #"^(\d+)" :integerConstant
   #"^\"((?!\").*)\"" :stringConstant

   #"^([A-Za-z_][A-Za-z0-9_]*)" :identifier})

(defn- remove-match
  "Removes the match m from the beginning of s"
  [s m]
  (.substring s (.length m)))


(defn extract-token
  "Extracts token from s if the provided rule is the next match
  for s. Returns vector of the new string after extraction and token
  of the rule's type. Returns nil if the rule is not the next match."
  [s [r t]]
  (if-let [[match value] (re-find r s)]
    [(remove-match s match) (Token. t value)]
    nil))
