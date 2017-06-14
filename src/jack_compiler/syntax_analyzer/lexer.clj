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


(defn extract-token-with-rule
  "Extracts token from s if the provided rule is the next match
  for s. Returns vector of the new string after extraction and token
  of the rule's type. Returns nil if the rule is not the next match."
  [s [r t]]
  (if-let [[match value] (re-find r s)]
    [(remove-match s match) (Token. t value)]
    nil))

(defn extract-token-with-rules
  "Extracts token from s for the first rule in rs that matches
  s. Returns vector of new string after extraction and then token
  of the type of the rule that matched. Returns nil if no matches found."
  [s rs]
  (some (partial extract-token-with-rule s) rs))

(defn extract-token
  "Extracts the next token from s based on Jack's syntax rules.
  Returns vector of new string after extraction and the extracted token.
  Returns no rule matched, indicating some syntax error in s."
  [s]
  (extract-token-with-rules s token-rules))

(defn token-all-seq
  "Generates a lazy seq of all tokens from s"
  [s]
  (if-let [[nxt t] (extract-token s)]
    (cons t (lazy-seq (token-seq nxt)))
    (ex-info "Syntax error" {:source s})))

(defn whitespace?
  "Checks whether token t is a whitespace"
  [t]
  (= :whitespace (.type t)))

(def not-whitespace? (complement whitespace?))

(defn token-seq
  "Generates a lazy seq of tokens from s.
  Skips whitespace tokens"
  [s]
  (filter not-whitespace? (token-seq s))
