(ns clirc.glucose
  (:require [clojure.core.match :refer [match]]))

(declare expand-inline-sttmt
         expand-inline-funcall
         expand-inline-arg
         expand-inline-arg-list)

(defn make-newvar-fn
  ([counter] (make-newvar-fn counter "$temp__" "__"))
  ([counter prefix sufix]
   (fn []
     (let [i (swap! counter inc)]
       (symbol (str prefix i sufix))))))

(defn expand-inline
  [prog]
  (let [count (atom 0)
        newvar (make-newvar-fn count)
        aux (fn [acc sttmt]
              (into [] (concat acc
                               (expand-inline-sttmt sttmt newvar))))]
    (reduce aux [] prog)))


(defn expand-inline-sttmt
  [sttmt newvar]
  (match [sttmt]
    [(['set! lhs rhs] :seq)]
    (let [ex-funcall (expand-inline-funcall rhs newvar)]
      (conj (:prep ex-funcall)
            `(set! ~lhs ~(:rawcall ex-funcall))))))


(defn expand-inline-funcall
  "Receives a function call with, possibly, inline function call as arguments, and
  returns its translation to raw Clirc code. The result is a dictionary with
  three keys.

  - `:prep` the <em>preparation code</em> that must be executed before the
  <em>raw call</em> is executed.

  - `:rawcall` the <em>raw function call</em>, i.e., the function call with all
  inline function calls removed and replaced by temporary variables.
  "
  ([funcall newvar]
   (match [funcall]
     [([func & args] :seq)]
     (let [ex-args (expand-inline-arg-list args newvar)
           rawcall `(~func ~@(:vars ex-args))]
       {:prep (:prep ex-args)
        :rawcall rawcall}))))


(defn expand-inline-arg-list
  "Receives translation parameters and a function call argument, and returns the
  updated translation information. The input parameters are:

  - `args` a sequence of arguments to a function call. Arguments can be
  variables or an inline function calls.

  - `newvar` a function that generates new variables.

  The result is a map with the following keys:

  - `:prep` an array of statements that must be execute before the function call
  that uses the arguments.

  - `:vars` an array of variables that will replace `args` in the main function
  call.
  "
  [args newvar]
  (let [ex-args (for [arg args] (expand-inline-arg arg newvar))]
        {:prep (into [] (apply concat (for [xarg ex-args] (:prep xarg))))
         :vars (into [] (for [xarg ex-args] (:var xarg)))}))


(defn expand-inline-arg
  "Receives a function call argument and a counter, and returns a map with
  translation information. The input parameters are:

  - `arg` an argument to a function call. It can be a variable or an inline
  function call.

  - `newvar` a function that generates new variables.

  The result is a map with the following keys:

  - `:prep` an array with the code that must be execute before the function call
  that uses the argument. If `arg` was a variable, `:prep` will be the empty
  array.

  - `:var` the variable that will replace `arg` in the main function call. If
  `arg` is already a variable, then it will be returned unchanged.
  "
  [arg newvar]
  (match [arg]
    [(var :guard symbol?)] {:prep [], :var var}
    [([func & args] :seq)] (let [ex-args (expand-inline-arg-list args newvar)
                                 ex-prep (:prep ex-args)
                                 ex-rawargs (:vars ex-args)
                                 temp-var (newvar)
                                 assign `(set! ~temp-var (~func ~@ex-rawargs))]
                             {:prep (conj ex-prep assign)
                              :var temp-var})
    ;;
    ))
