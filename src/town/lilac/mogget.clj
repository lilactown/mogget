(ns town.lilac.mogget
  (:refer-clojure :exclude [eval]))

(defn display
  [{:keys [stack] :as ctx}]
  (prn (peek stack))
  (assoc ctx :stack (pop stack)))

(defn -pop
  [stack]
  [(pop stack) (peek stack)])

(defn ->fn
  [f & {:keys [arity results]
        :or {arity 1
             results 1}}]
  (fn [{:keys [words] :as ctx}]
    (loop [n arity
           args ()
           stack (:stack ctx)]
      (if (pos? n)
        (if (seq stack)
          (recur
           (dec n)
           (conj args (peek stack))
           (pop stack))
          (throw (ex-info "Stack empty executing fn" {:f f :arity arity})))
        (assoc
         ctx :stack
         (if (= 1 results)
           (conj stack (apply f words args))
           ;; TODO throw exception if actual results > results
           (into stack (apply f words args))))))))


(defn ->sfn
  "pure stack fn"
  [f & {:keys [arity results] :as args}]
  (->fn (fn [_words & args] (apply f args)) args))

(declare eval-list)

(def default-words
  {'prn display

   ;; arithmetic
   'inc (->sfn inc)
   'dec (->sfn dec)
   '+ (fn [{:keys [stack] :as ctx}]
        (let [[stack x] (-pop stack)
              [stack y] (-pop stack)]
          (assoc ctx :stack (conj stack (+ x y)))))
   '- (->sfn - :arity 2)
   '* (->sfn * :arity 2)
   '/ (->sfn / :arity 2)

   ;; tests
   '< (->sfn < :arity 2)
   '> (->sfn > :arity 2)
   '= (->sfn = :arity 2)
   'even? (->sfn even?)

   ;; control flow
   'if (fn [{:keys [stack] :as ctx}]
         (let [[stack else] (-pop stack)
               [stack then] (-pop stack)
               [stack cond] (-pop stack)]
           (-> (assoc ctx :stack stack)
               (eval-list (if cond then else)))))

   ;; seqs
   'first (->sfn first)
   'second (->sfn second)
   'nth (->sfn nth :arity 2)
   'conj (->sfn conj :arity 2)
   'map (->fn
         (fn [words coll form]
           (map (fn [x]
                  (-> {:stack [x] :words words :mode :eval}
                      (eval-list form)
                      ;; TODO throw err if > 1 results on stack
                      :stack
                      first))
                coll))
         :arity 2)
   'filter (->fn
            (fn [words coll form]
              (filter (fn [x]
                        (-> {:stack [x] :words words :mode :eval}
                            (eval-list form)
                            ;; TODO throw err if > 1 results on stack
                            :stack
                            first))
                      coll))
            :arity 2)
   'reduce (->fn (fn [words coll init f]
                   (reduce (fn [res x]
                             (-> {:stack [res x] :words words :mode :eval}
                                 (eval-list f)
                                 :stack
                                 ;; TODO throw if > 1 result
                                 first))
                           init
                           coll))
                 :arity 3)
   'sort (->sfn sort)

   ;; combinators
   'bi (->fn
        (fn [words x p q]
          [(-> {:stack [x] :words words :mode :eval}
               (eval-list p)
               (:stack)
               ;; TODO throw err if > 1 results on stack
               (first))
           (-> {:stack [x] :words words :mode :eval}
               (eval-list q)
               (:stack)
               (first))])
        :arity 3 :results 2)
   'dip (fn [{:keys [stack] :as ctx}]
          (let [[stack form] (-pop stack)
                [stack x] (-pop stack)]
            (-> (assoc ctx :stack stack)
                (eval-list form)
                (update :stack conj x))))
   'apply (fn [{:keys [stack] :as ctx}]
            (let [[stack form] (-pop stack)]
              (eval-list (assoc ctx :stack stack) form)))
   'spread (->sfn (fn [seq] seq) :results :many)

   ;; shuffle
   'swap (->sfn (fn [x y] [y x]) :arity 2 :results 2)
   'dup (->sfn (fn [x] [x x]) :arity 1 :results 2)
   'rm (->sfn (fn [x y] x) :arity 2 :results 1)
   'nip (->sfn (fn [x y] y) :arity 2 :results 1)
   'over (->sfn (fn [x y] [x y x]) :arity 2 :results 3)
   'pick (->sfn (fn [x y z] [x y z x]) :arity 3 :results 4)


   'define (fn [{:keys [stack words] :as ctx}]
             (let [[stack form] (-pop stack)
                   sym (first form)
                   expr (second form)]
               (assoc
                ctx
                :stack stack
                :words (assoc words sym
                              (fn [ctx]
                                (eval-list ctx expr))))))})

(defprotocol IEval
  (-eval [x stack]))

(extend-protocol IEval
  Object
  (-eval [o ctx] (update ctx :stack conj o))

  clojure.lang.PersistentList
  (-eval [lst ctx]
    (if (= 'quote (first lst))
      (update ctx :stack conj (second lst))
      (update ctx :stack conj lst)))

  clojure.lang.Symbol
  (-eval [sym {:keys [words mode] :as ctx}]
    (cond
      (= 'quote sym) (assoc ctx :mode :quote)
      (= :quote mode) (-> ctx
                          (update :stack conj sym)
                          (assoc :mode :eval))
      :else (if-let [f (get words sym)]
              (f ctx)
              (throw (ex-info "word not found" {:word sym}))))))

(def prelude
  '((doto (over (apply) dip)) define
    (each (map spread)) define
    (when (() if)) define))

(defn eval-list
  [ctx list]
  (reduce
    (fn [ctx x]
      (-eval x ctx))
    ctx
    (concat prelude list)))

(defmacro eval
  [& form]
  `(:stack
    (eval-list
     {:stack [] :words default-words :mode :eval}
     '~form)))

^:rct/test
(comment
  ;; basic data
  (eval 1 2 3)

  (eval [1] [2] [3])

  ;; arithemetic
  (eval 1 2 3 + +)
  ;; => [6]
  (eval 2 1 -)
  ;; => [1]
  (eval 1 inc)
  ;; => [2]

  ;; testing
  (eval 1 1 =)
  ;; => [true]
  (eval [1 2 3] [1 2 3] =)
  ;; => [true]
  (eval 1 2 =)
  ;; => [false]

  (eval 1 2 <)
  ;; => [true]
  (eval 2 1 <)
  ;; => [false]

  (eval 2 1 >)
  ;; => [true]
  (eval 1 2 >)
  ;; => [false]

  ;; control flow
  (eval 0 1 2 < (inc) (dec) if)
  ;; => [1]
  (eval 0 1 2 > (inc) (dec) if)
  ;; => [-1]
  (eval 0 1 2 < (inc) when)
  ;; => [1]
  (eval 0 1 2 > (inc) when)
  ;; => [0]

  ;; seq
  (eval [1 2 3] first)
  ;; => [1]
  (eval [1 2 3] second)
  ;; => [2]
  (eval [1 2 3] 2 nth)
  ;; => [3]
  (eval [1 2 3] 4 conj)
  ;; => [[1 2 3 4]]
  (eval [1 2 3] (inc) map)
  ;; => [(2 3 4)]
  (eval [1 2 3] (2 +) map)
  ;; => [(3 4 5)]
  (eval [1 2 3] (even?) filter)
  ;; => [(2)]
  (eval [1 2 3] 0 (+) reduce)
  ;; => [6]
  (eval [1 2 3] (inc) each)
  ;; => [2 3 4]
  (eval [1 4 2 3] sort)
  ;; => [(1 2 3 4)]


  ;; shuffle
  (eval 1 2 swap)
  ;; => [2 1]
  (eval 1 2 dup)
  ;; => [1 2 2]
  (eval 1 2 rm)
  ;; => [1]
  (eval 1 2 nip)
  ;; => [2]
  (eval 1 2 over)
  ;; => [1 2 1]
  (eval 0 1 2 3 pick)
  ;; => [0 1 2 3 1]


  ;; combinator
  (eval 1 (inc) (inc) bi)
  ;; => [2 2]
  (eval 1 2 3 (+ +) apply)
  ;; => [6]
  (eval 1 3 (inc) dip)
  ;; => [2 3]
  (eval
   4
   (2 +) doto
   (2 *) doto
   (2 /) doto)
  ;; => [6 8 2 4]
  (eval [1 2 3] spread)
  ;; => [1 2 3]


  ;; fancy
  (eval (+) 2 conj [1 2 3] swap map)
  ;; => [(3 4 5)]

  ;; define
  (eval (sq (dup *)) define 2 sq)
  ;; => [4]
  (eval (sq (dup *)) define [1 2 3] (sq) map)
  ;; => [(1 4 9)]
  (eval (sq (dup *)) define
        3 (sq) (sq) bi)
  ;; => [9 9]
  )


(require '[com.mjdowney.rich-comment-tests :as rct])
(rct/run-ns-tests! *ns*)
