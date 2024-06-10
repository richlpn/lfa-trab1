(ns clirc.core
  (:gen-class)
  (:require [clojure.core.match :refer [match]]
            [clirc.bool-logic :refer [map->bitvec]]))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))


;;; There are 4 dialects of the language Clirc (CIRC): AON, IZO, NAND and NOR.
;;; The only difference between then is the set of predefined functions that
;;; they recognize:
;;;
;;; - AON: and, or, not
;;; - IZO: if, zero, one
;;; - NAND: nand
;;; - NOR: nor
;;;
;;; So there is a general function called `eval-prog`, that evaluates program in
;;; all dialects. This function receives as first argument, a map with the
;;; predefined functions. Each predefined function will be defined by a symbol
;;; -- the name of the function -- as key, and a record (map) with keys for the
;;; `:arity` (number of arguments) of the function and the `:body` of the
;;; function.

(declare eval-prog)

(def aon-funcs
  "Set of predefined functions for the AON dilect."
  {'and {:arity 2 :body #(min %1 %2)}
   'or  {:arity 2 :body #(max %1 %2)}
   'not {:arity 1 :body #(- 1 %1)}})

(defn eval-prog-aon
  "Helper function to evaluate AON-Clirc programs. See also [[eval-prog]]."
  [prog input]
  (eval-prog aon-funcs prog input))


(def izo-funcs
  "Set of predefined functions for the IZO dilect."
  {'if   {:arity 3 :body #(if (= %1 1) %2 %3)}
   'zero {:arity 1 :body (fn [_] 0)}
   'one  {:arity 1 :body (fn [_] 1)}})

(defn eval-prog-izo
  "Helper function to evaluate IZO-Clirc programs. See also [[eval-prog]]."
  [prog input]
  (eval-prog izo-funcs prog input))


(def nand-funcs
  "Set of predefined functions for the NAND dilect."
  {'nand {:arity 2 :body #(- 1 (* %1 %2))}})

(defn eval-prog-nand
  "Helper function to evaluate AON-Clirc programs. See also [[eval-prog]]."
  [prog input]
  (eval-prog nand-funcs prog input))


(def nor-funcs
  "Set of predefined functions for the NOR dilect."
  {'nor {:arity 2 :body #(- 1 (max %1 %2))}})

(defn eval-prog-nor
  "Helper function to evaluate AON-Clirc programs. See also [[eval-prog]]."
  [prog input]
  (eval-prog nor-funcs prog input))


(declare eval-funcall)

(defn eval-prog
  "Evaluates, i.e., executes, Clirc programs for a given input with a given
  set of predefined functions.

  - `predefs` is the set of predefined function. It defines the dialect of
  the Clirc language being used.

  - `prog` is the program to be evaluated, i.e., a sequence of assignments.
  The only kind of statement accepted in the core evaluator is the assignment.
  The syntax of the assignment is `(set! lhs rhs)`, where `lhs` stands for
  *left hand side*, and `rhs` stands for *right hand side*. The `lhs` can be
  a variable (symbol) or the output. The `rhs` is a function call. See also
  [[eval-funcall]].

  - `input` is an array of zeros and ones making the input of the program.

  The function will return a map with the output generatde by the execution
  of `prog` for the given `input` values. Only the output indexes that have
  been assigned values in `prog` will be present in the output map.
  "
  [predefs prog input]
  (loop [sttmts prog, env (assoc predefs :in input :out {})]
    (if (empty? sttmts)
      (map->bitvec (:out env))
      (let [sttmt (first sttmts)
            lhs (nth sttmt 1)
            rhs (nth sttmt 2)
            value (eval-funcall rhs env)]
        (match [lhs]
          ;; Assignment to variable
          [(var :guard symbol?)]
          (recur (rest sttmts) (assoc env var value))
          ;; Assignment to output
          [([:out n] :seq)]
          (recur (rest sttmts) (assoc-in env [:out n] value))
          ;; Se for outra coisa
          :else (throw (Error. "LHS inválido.")))))))


(declare eval-expr)

(defn eval-funcall
  "Evaluates a predefined function call.

  - `funcall` is the expression representing the function call, with the
  syntax `(fname arg1 ... argn)`, where `fname` is the name (symbol) of a
  predefined function, and the `arg`s are variables, inputs or outputs.
  The inputs are defined as `(:in n)`, and the output as `(:out n)`, where
  `n` is a non-negative integer.

  - `env` is the environment where the function will be evaluated.

  Returns the numerical result (zero or one) resulting from the function call
  evaluation.
  "
  [funcall env]
  (let [fname (first funcall)
        args (rest funcall)
        func (get env fname)]
    (cond
      ;;
      (nil? func)
      (throw (ex-info "Undefined function." {:fname fname :args args}))
      ;;
      (not= (count args) (:arity func))
      (throw (ex-info "Wrong number of arguments." {:fname fname :args args}))
      ;;
      :else
      (let [arg-values (map #(eval-expr %1 env) args)]
        (apply (:body func) arg-values)))))


(defn eval-expr
  "Evaluates expressions in the *Clirc* language with a given environment.

  - `expr` the expression to be evaluated. The valid expressions are: `nil`,
  varibles, inputs, and outputs.

  - `env` the environment used to evaluate the expressions.

  Returns the numeric value (zero or one) of the expression."
  [expr env]
  (match [expr]
    [nil] nil
    [([:in n] :seq)] (get-in env [:in n])
    [([:out n] :seq)] (get-in env [:out n])
    [(x :guard symbol?)] (get env x)
    :else (throw (ex-info "Invalid expression."
                          {:expr expr :env env}))))
