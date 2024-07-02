(ns clirc.probl2
  (:require [clirc.glucose :refer [make-newvar-fn]]
            [clojure.core.match :refer [match]]))

;; TODO: Better DOCS
;; TODO: readme.md
;; TODO: Check for recurseve if

(defn assing-var-temp
  [dict lkey prefix tmp]
  (if (nil? (get dict lkey))
    (assoc dict lkey {prefix tmp})
    (assoc dict lkey (assoc (get dict lkey) prefix tmp))))

(defn assing-if-sttm
  "Transforma um 'statement' de um bloco if (then/else) em uma variáveis temporária.
   O resultado é um dicionário (pieces) com as chaves :prep e :var-map.

  - `count` Contador de variáveis temporárias criadas.

  - `lhs` Variável sendo <em>setada</em> no statement.

  - `rhs` Expressão sendo <em>computada</em> no statement.

  - `prefix` Prefixo das variávies temporárias.

  - `pieces` HashMap que contem as chaves :prep e :var-map. 
  São usadas respectivamente para armazenar as chamadas resultantes e relacionar a variável origial a suas temporárias.
  "
  [count lhs rhs prefix pieces]
  (let [newvar ((make-newvar-fn count (str lhs prefix) ""))
        ;; Relaciona a váriavel com a sua temporária de acordo com o prefixo
        new-var-map (assing-var-temp (:var-map pieces) lhs prefix newvar)
        ;;Concatena os resultados anteriores desse prefixo com a nova chamada da váriavel temporária
        new-prep (assoc (:prep pieces) prefix (into (get (:prep pieces) prefix) `((set! ~newvar ~rhs))))]
    {:prep new-prep :var-map new-var-map}))

(defn assing-if-block
  [block prefix count pieces]
  (reduce #(match [%2] [(['set! lhs rhs] :seq)]
                  (assing-if-sttm count lhs rhs prefix %1))
          pieces block))

(defn assing-rawcall
  [expr origin vars]
  (let [temp1 (get vars "$then")
        temp2 (get vars "$else")
        then (if (nil? temp1) origin temp1)
        otherwise (if (nil? temp2) origin temp2)]
    `(set! ~origin (IFF ~expr ~then ~otherwise))))

(defn assing-if-rawcalls
  [expr pieces]
  (let [rawcalls (reduce #(conj %1 (assing-rawcall expr (nth %2 0) (nth %2 1))) (list) (:var-map pieces))
        prep (concat (get (:prep pieces) "$then") (get (:prep pieces) "$else"))]
    (into rawcalls prep)))

(defn expand-if-structure
  [expr then otherwise count]
  (->> {:prep {"$then" [] "$else" []} :var-map {}}
       (assing-if-block then "$then" count)
       (assing-if-block otherwise "$else" count)
       (assing-if-rawcalls expr)))


(defn expand-if
  ([sttmt] (expand-if sttmt (atom 0)))
  ([sttmt count]
   (match [sttmt]
     [(['if expr then otherwise] :seq)] ;; É uma estrutura if? 
     (let [sttmts
           (expand-if-structure expr then otherwise count)]
       {:sttmt sttmts :count count}))))

(def ifs '((if (:in 0)
             [(set! a (and (:in 1) (:int 2)))
              (set! b (or (:in 1) (:in 2)))]
             [(set! a (xor (:in 0) (:int 2)))
              (set! b (nand (:in 0) (:in 2)))
              (set! c (nand (:in 0) (:in 2)))])))

(def iff (nth ifs 0))
