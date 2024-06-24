(ns probl2.probl2
  (:require [clojure.core.match :refer [match]]))

(defn expand-if-sstmt 
  [expr then otherwise count] 
  (match [(_ :guard list?) (_ :guard list?) (_ :guard list?)]
    (let [count2 count
          then-block (assing-if-block "then" then, count)
          otherwise-block (assing-if-block "else" otherwise, count2)]
          )
    :else (println "estrutura if está errada")
  )
)


(defn expand-if
  [sttmt count]
  (match [sttmt]
    [(['if expr then otherwise] :seq)] ;; É uma estrutura if? 
      (let [ex-cond 
        (expand-if-structure expr then otherwise count)]
        {:sttmt (conj (:prep ex-cond) `(:rawcalls ex-cond))
        :count (:count ex-cond)})))