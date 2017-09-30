;; from https://gist.github.com/codification/1984857

(ns rojure.proc
 (:import [java.lang ProcessBuilder])
 (:use [clojure.java.io :only [reader writer]]))

(defn spawn [& args]
 (let [process (-> (ProcessBuilder. args)
                   (.start))]
  {:out (-> process
            (.getInputStream)
            (reader))
   :err (-> process
            (.getErrorStream)
            (reader))
   :in (-> process
           (.getOutputStream)
           (writer))
   :process process}))
