(ns clirc.sugar
  (:require [clojure.core.match :refer [match]]))

(declare expand-inline-assign
         expand-inline-funcall
         expand-inline-arg
         expand-inline-arg-list)

(defn expand-inline
  [prog]
  (letfn [(expand-assign-acc [acc assign]
            (let [ex-assign (expand-inline-assign assign (:count acc))]
              {:sttmts (into [] (concat (:sttmts acc) (:sttmts ex-assign)))
               :count (:count ex-assign)}))]
    (->> prog
         (reduce expand-assign-acc {:sttmts [], :count 0})
         (:sttmts))))


(defn expand-inline-assign
  [assign count]
  (match [assign]
    [(['set! lhs rhs] :seq)] (let [ex-funcall (expand-inline-funcall rhs count)]
                               {:sttmts (conj (:prep ex-funcall)
                                              `(set! ~lhs ~(:rawcall ex-funcall)))
                                :count (:count ex-funcall)})))


(defn expand-inline-funcall
  "Receives a function call with, possibly, inline function call as arguments, and
  returns its translation to raw Clirc code. The result is a dictionary with
  three keys.

  - `:prep` the <em>preparation code</em> that must be executed before the
  <em>raw call</em> is executed.

  - `:rawcall` the <em>raw function call</em>, i.e., the function call with all
  inline function calls removed and replaced by temporary variables.

  - `:count` the numeric ID of the last temporary variable created for the
  preparation code."
  ([funcall]
   (expand-inline-funcall funcall 0))
  ([funcall count]
   (match [funcall]
     [([func & args] :seq)]
     (let [ex-args (expand-inline-arg-list args count)
           rawcall `(~func ~@(:vars ex-args))]
       {:prep (:prep ex-args)
        :rawcall rawcall
        :count (:count ex-args)}))))


(defn expand-inline-arg-list
  "Receives translation parameters and a function call argument, and returns the
  updated translation information. The incoming (and the outgoing) translation
  information is a map with the following keys:

  - `:prep` an array of statements that must be execute to define the values of
  the temporary variables.

  - `:vars` the array variable names corresponding to the translated function
  call arguments.

  - `:count` the numeric ID of the last generated temporary variable.
  "
  [args count]
  (letfn [(aux [acc arg]
            (let [ex-arg (expand-inline-arg arg (:count acc))]
              {:prep (into [] (concat (:prep acc) (:prep ex-arg)))
               :vars (conj (:vars acc) (:var ex-arg))
               :count (:count ex-arg)}))]
    (reduce aux {:prep [], :vars [], :count count} args)))


(defn expand-inline-arg
  "Receives a function call argument and a counter, and returns a map with
  translation information. The input parameters are:

  - `arg` an argument to a function call. It can be a variable or an inline
  function call.

  - `count` a counter for the number of temporary variables already created.

  The result is a map with the following keys:

  - `:prep` an array with the code that must be execute before the function call
  that uses the argument. If `arg` was a variable, `:prep` will be the empty
  array.

  - `:var` the variable that will replace `arg` in the main function call. If
  `arg` is already a variable, then it will be returned unchanged.

  - `:count` the numeric ID of the last generated temporary variable. If `arg`
  is already a variable, the input argument `count` will be returned unchanged.
  "
  [arg count]
  (match [arg]
    [(var :guard symbol?)] {:prep [], :var var, :count count}
    [([func & args] :seq)] (let [ex-args (expand-inline-arg-list args count)
                                 ex-prep (:prep ex-args)
                                 ex-rawargs (:vars ex-args)
                                 ex-count (inc (:count ex-args))
                                 temp-var (symbol (str "$temp__" ex-count "__"))
                                 assign `(set! ~temp-var (~func ~@ex-rawargs))]
                             {:prep (conj ex-prep assign)
                              :var temp-var
                              :count ex-count})
    ;;
    ))

;; ====================== Meu código =======================

(declare expand-if
         expand-if-block
         expand-if-block-sstmt)

(defn expand-if 
  [prog]
  (letfn [(expand-cond [acc sttmt]
          (let [pro_sttmt (expand-if-block sstmt 
              (:count acc))]
              {:sttmt (into [] (concat (:sttmt acc) (:sttmt pro_sttmt)))
              :count (:count pro_sttmt)}))])

  (->> prog
  (reduce expand-if-block {:sttmt [] :count 0})))

(defn expand-if-block
  [sttmt count]
  (match [sttmt]
    [(['if expr then otherwise] :seq)] ;; É uma estrutura if? 
      (let [ex-cond 
        (expand-if-block-sstmt expr then otherwise count)]
        {:sttmt (conj (:prep ex-cond) `(:rawcalls ex-cond))
        :count (:count ex-cond)})))

(defn expand-if-block-sstmt
[expression then otherwise count]
{:prep ["code1" "code2"] :count 0 :rawcalls ["code3" "code4"]}
)