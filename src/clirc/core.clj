(ns clirc.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

;; # Compute CMP:{0,1}^4-->{0,1}
;; # CMP(X)=1 iff 2X[0]+X[1] > 2X[2] + X[3]
;; temp_1 = NOT(X[2])
;; temp_2 = AND(X[0],temp_1)
;; temp_3 = OR(X[0],temp_1)
;; temp_4 = NOT(X[3])
;; temp_5 = AND(X[1],temp_4)
;; temp_6 = AND(temp_5,temp_3)
;; Y[0] = OR(temp_2,temp_6)


(def cmp '[(set! temp1 (not (:in 2)))
           (set! temp2 (and (:in 0) temp1))
           (set! temp3 (or (:in 0) temp1))
           (set! temp4 (not (:in 3)))
           (set! temp5 (and (:in 1) temp4))
           (set! temp6 (and temp5 temp3))
           (set! (:out 0) (or temp2 temp6))])

(defn aon-operand-eval
  [operand env]
  (cond
    ;; Operando nulo
    (nil? operand) nil
    ;; Operando é variável
    (symbol? operand) (get env operand)
    ;; Operando é input
    (and (list? operand) (= (first operand) :in))
    (get-in env [:in (nth operand 1)])
    ;; Operando é output
    (and (list? operand) (= (first operand) :out))
    (get-in env [:out (nth operand 1)])
    ;; Caso contrário
    :else (throw (Error. "Operando inválido."))))

(defn aon-func-eval
  [expr env]
  (let [func (nth expr 0)
        op1 (nth expr 1)
        op2 (nth expr 2 nil)
        val1 (aon-operand-eval op1 env)
        val2 (aon-operand-eval op2 env)]
    (case func
      and (* val1 val2)
      or (max val1 val2)
      not (- 1 val1))))


(defn aon-eval [prog input]
  (loop [prog1 prog env {:in input :out {}}]
    (cond
      (empty? prog1) (get env :out)
      :else (let [sttmt1 (first prog1)
                  lhs (nth sttmt1 1)
                  rhs (nth sttmt1 2)
                  value (aon-func-eval rhs env)]
              (cond
                ;; Se for variável
                (symbol? lhs)
                (recur (rest prog1) (assoc env lhs value))
                ;; Se for :out
                (and (list? lhs) (= (first lhs) :out))
                (recur (rest prog1) (assoc-in env [:out (nth lhs 1)] value))
                ;; Se for outra coisa
                :else (throw (Error. "LHS inválido.")))))))

