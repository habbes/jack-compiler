(ns jack-compiler.file
  (:require [clojure.java.io :as io]
            [clojure.string :as s])
  (:import [java.nio.file Paths]))


(defn- remove-ext
  "Removes file extension from path"
  [path]
  (s/replace path
             #"(\.\w+)$"
             ""))

(defn jack-file?
  "Checks whether the specified path is a .jack file"
  [path]
  (s/ends-with? path ".jack"))

(defn dir?
  "Checks whether the specified path is a directory"
  [path]
  (.isDirectory (io/file path)))

(defn get-filtered-files
  "Returns a set of files from the specified directory,
  filered through f"
  [path f]
  (let [dir (io/as-file path) files (.listFiles dir)]
    (->> files
         (map #(.getPath %))
         (filter f)
         (into #{}))))

(defn get-jack-files
  "Returns a set of jack files from the specified directory"
  [path]
  (get-filtered-files path jack-file?))

(defn get-output-path-for-file
  "Gets the output path of the specified extension based on
  the input file's path"
  [input-path ext]
  (-> input-path
      (io/as-file)
      (.getPath)
      remove-ext
      (str ext)))

(defn get-class-name
  "Gets the class name based on the input jack file"
  [input-path]
  (-> (.getName (io/file input-path))
      (remove-ext)))
