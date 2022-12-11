(ns town.lilac.mogget
  (:refer-clojure :exclude [eval])
  (:require
   [clojure.string :as str]
   [edamame.core :as eda]))


;; http://cninja.blogspot.com/2011/02/clojure-partition-at.html#comments
(defn partition-with
  "Like partition-by but will start a new run when f returns true"
  [f coll]
  (lazy-seq
   (when-let [s (seq coll)]
     (let [run (cons (first s) (take-while #(not (f %)) (rest s)))]
       (cons run (partition-with f (drop (inc (count run)) s)))))))

(defn -pop
  [stack]
  [(pop stack) (peek stack)])

(defn -popm
  [n stack]
  (loop [n n
         xs []
         stack stack]
    (if (pos? n)
      (if (seq stack)
        (let [x (peek stack)
              stack (pop stack)]
          (recur (dec n) (conj xs x) stack))
        (throw (ex-info "Stack empty" {:stack stack})))
      [stack xs])))

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
  {;; i/o
   'prn (fn display
          [{:keys [stack] :as ctx}]
          (prn (peek stack))
          (assoc ctx :stack (pop stack)))
   'slurp (->sfn slurp)
   'spit (->sfn (fn [contents file]
                  (spit file contents)
                  []) :arity 2 :results :none)

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
   'max (->sfn max :arity 2)
   'parse-long (->sfn parse-long)

   ;; tests
   '< (->sfn < :arity 2)
   '> (->sfn > :arity 2)
   '= (->sfn = :arity 2)
   '<= (->sfn <= :arity 2)
   '>= (->sfn >= :arity 2)
   'and (->sfn #(and %1 %2) :arity 2)
   'or (->sfn #(or %1 %2) :arity 2)
   'even? (->sfn even?)
   'odd? (->sfn odd?)
   'some? (->sfn some?)
   'nil? (->sfn nil?)

   ;; control flow
   'if (fn [{:keys [stack] :as ctx}]
         (let [[stack else] (-pop stack)
               [stack then] (-pop stack)
               [stack cond] (-pop stack)]
           (-> (assoc ctx :stack stack)
               (eval-list (if cond then else)))))
   'case (fn [{:keys [stack] :as ctx}]
           (let [[stack table] (-pop stack)
                 [stack v] (-pop stack)]
             (-> (assoc ctx :stack stack)
                 (eval-list (get table v)))))
   '? (fn [{:keys [stack] :as ctx}]
        (let [[stack table] (-pop stack)
              ctx (assoc ctx :stack stack)]
          (loop [table (partition-all 2 table)]
            (if-let [cond (first table)]
              (if (second cond) ; not a default
                (let [ctx (eval-list ctx (first cond))]
                  (if (-> ctx :stack peek) ; cond result success
                    (eval-list (update ctx :stack pop) (second cond))
                    (recur (rest table))))
                (eval-list ctx (first cond)))
              ctx))))

   ;; seqs
   'first (->sfn first)
   'second (->sfn second)
   'nth (->sfn nth :arity 2)
   'conj (->sfn conj :arity 2)
   'into (->sfn into :arity 2)
   'concat (->sfn concat :arity 2)
   'reverse (->sfn reverse)
   'collect (fn [{:keys [stack] :as ctx}]
              (let [[stack n] (-pop stack)
                    [stack xs] (-popm n stack)]
                (assoc ctx :stack (conj stack xs))))
   'map (->fn
         (fn [words coll form]
           (map (fn [x]
                  (-> {:stack [x] :words words :mode :eval}
                      (eval-list form)
                      ;; TODO throw err if > 1 results on stack
                      :stack
                      peek))
                coll))
         :arity 2)
   'filter (->fn
            (fn [words coll form]
              (filter (fn [x]
                        (-> {:stack [x] :words words :mode :eval}
                            (eval-list form)
                            ;; TODO throw err if > 1 results on stack
                            :stack
                            peek))
                      coll))
            :arity 2)
   'reduce (->fn (fn [words coll init f]
                   (reduce (fn [res x]
                             (-> {:stack [res x] :words words :mode :eval}
                                 (eval-list f)
                                 :stack
                                 ;; TODO throw if > 1 result
                                 peek))
                           init
                           coll))
                 :arity 3)
   'sort (->sfn sort)
   'range (->sfn range :arity 2)
   'split-with (->fn (fn [words coll f]
                       (split-with
                        #(-> {:stack [%] :words words :mode :eval}
                             (eval-list f) :stack peek)
                        coll))
                     :arity 2)
   'take (->sfn (fn [coll n] (take n coll)) :arity 2)
   'drop (->sfn (fn [coll n] (drop n coll)) :arity 2)
   'partition-with (->fn (fn [words coll f]
                          (partition-with
                           #(-> {:stack [%] :words words :mode :eval}
                                (eval-list f) :stack peek)
                           coll))
                         :arity 2)


   ;; assocs
   'get (->sfn get :arity 2)
   'get-in (->sfn get-in :arity 2)
   'assoc (->sfn assoc :arity 3)
   'dissoc (->sfn dissoc :arity 2)
   'assoc-in (->sfn assoc-in :arity 3)
   'merge (->sfn merge :arity 2)
   'update (fn [{:keys [stack] :as ctx}]
             (let [[stack f] (-pop stack)
                   [stack k] (-pop stack)
                   [stack m] (-pop stack)
                   v (get m k)
                   ctx (eval-list (assoc ctx :stack (conj stack v)) f)
                   m (assoc m k (-> ctx :stack peek))]
               (-> ctx
                   (update :stack pop)
                   (update :stack conj m))))
   'zipmap (->sfn (fn [ks vs] (zipmap ks vs))
                  :arity 2)

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
   'call (fn [{:keys [stack] :as ctx}]
           (let [[stack form] (-pop stack)]
             (eval-list (assoc ctx :stack stack) form)))
   'apply (->fn (fn [words args f]
                  (:stack (eval-list {:stack args :words words :mode :eval} f)))
                :arity 2 :results :many)
   'spread (->sfn (fn [seq] seq) :results :many)

   ;; shuffle
   'swap (->sfn (fn [x y] [y x]) :arity 2 :results 2)
   'dup (->sfn (fn [x] [x x]) :arity 1 :results 2)
   'rm (->sfn (fn [x y] x) :arity 2 :results 1)
   'nip (->sfn (fn [x y] y) :arity 2 :results 1)
   'over (->sfn (fn [x y] [x y x]) :arity 2 :results 3)
   'pick (->sfn (fn [x y z] [x y z x]) :arity 3 :results 4)


   ;; string
   'str (->sfn str :arity 2)
   'str/split (->sfn str/split :arity 2)

   ;; interop & defining
   'clj (fn [{:keys [stack] :as ctx}]
           (let [[stack n] (-pop stack)
                 [stack sym] (-pop stack)
                 [stack args] (-popm n stack)
                 f (resolve sym)
                 result (apply f args)]
             (assoc ctx :stack (conj stack result))))
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
    (if-let [f (get words sym)]
      (f ctx)
      (throw (ex-info "word not found" {:word sym})))))

(def prelude
  '((doto (over (call) dip)) define
    (each (map spread)) define
    (when (() if)) define))

(defn eval-list
  ([list] (eval-list {:stack [] :words default-words :mode :eval} list))
  ([ctx list]
   (reduce
    (fn [ctx x]
      (-eval x ctx))
    ctx
    (concat prelude list))))

(defn eval-string
  ([s] (eval-string {:stack [] :words default-words :mode :eval} s))
  ([ctx s] (eval-list ctx (eda/parse-string-all s {:regex true}))))

(defn eval-file
  ([f] (eval-file {:stack [] :words default-words :mode :eval} f))
  ([ctx f] (eval-string (slurp f))))

(defn run-file!
  [{:keys [file]}]
  (-> file eval-file :stack prn))

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
  (eval 1 2 max)
  ;; => [2]
  (eval "22" parse-long)
  ;; => [22]

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

  (eval true true and)
  ;; => [true]
  (eval true false and)
  ;; => [false]
  (eval false false and)
  ;; => [false]
  (eval true true or)
  ;; => [true]
  (eval true false or)
  ;; => [true]
  (eval false false or)
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
  (eval 0 :foo {:foo (inc) :bar (dec)} case)
  ;; => [1]
  (eval 0 :bar {:foo (inc) :bar (dec)} case)
  ;; => [-1]
  (eval 0 :baz {:foo (inc) :bar (dec)} case)
  ;; => [0]
  (eval 0 1 2
        ((<) (inc)
         (>) (dec)) ?)
  ;; => [1]
  (eval 0 2 1
        ((<) (inc)
         (>) (dec)) ?)
  ;; => [-1]
  (eval 0 1 1
        ((<) (inc)
         (>) (dec)) ?)
  ;; => [0 1 1]
  (eval 0 1 1
        ((<) (inc)
         (>) (dec)
         (+)) ?)
  ;; => [0 2]

  ;; seq
  (eval [1 2 3] first)
  ;; => [1]
  (eval [1 2 3] second)
  ;; => [2]
  (eval [1 2 3] 2 nth)
  ;; => [3]
  (eval [1 2 3] 4 conj)
  ;; => [[1 2 3 4]]
  (eval [1 2 3] [4 5 6] into)
  ;; => [[1 2 3 4 5 6]]
  (eval (1 2 3) (4 5 6) into)
  ;; => [(6 5 4 1 2 3)]
  (eval (1 2 3) (4 5 6) concat)
  ;; => [(1 2 3 4 5 6)]
  (eval 1 2 3 4 5 6 4 collect)
  ;; => [1 2 [6 5 4 3]]
  (eval 1 2 3 4 4 collect)
  ;; => [[4 3 2 1]]
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
  (eval 0 4 range)
  ;; => [(0 1 2 3)]
  (eval [1 2 3 4 5] (3 <=) split-with)
  ;; => [[(1 2 3) (4 5)]]
  (eval [1 2 3 4 5] 3 take)
  ;; => [(1 2 3)]
  (eval [1 2 3 4 5] 3 drop)
  ;; => [(4 5)]


  ;; assocs
  (eval {:a 1} :a get)
  ;; => [1]
  (eval {:a 1} :b 2 assoc)
  ;; => [{:a 1, :b 2}]
  (eval {:a 1} :a (inc) update)
  ;; => [{:a 2}]
  (eval {:a 1} {:b 2} merge)
  ;; => [{:a 1, :b 2}]
  (eval {:a 1} [:b 2] conj)
  ;; => [{:a 1, :b 2}]
  (eval [:one :two] [1 2] zipmap)
  ;; => [{:one 1, :two 2}]


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
  (eval (1 2 3) (+ +) apply)
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


  ;; clj
  (eval 1 2 3 '+ 3 clj)
  ;;(eval 0 1 (2 3 '+ 2) (call) )

  ;; define
  (eval (sq (dup *)) define 2 sq)
  ;; => [4]
  (eval (sq (dup *)) define [1 2 3] (sq) map)
  ;; => [(1 4 9)]
  (eval (sq (dup *)) define
        3 (sq) (sq) bi)
  ;; => [9 9]
  )

^:rct/test
(comment
  ;; AoC 2022 day 1
  (-> (slurp "example.mog")
      (eval-string)
      :stack)
  ;; => [24000 45000]
  )


(require '[com.mjdowney.rich-comment-tests :as rct])
(rct/run-ns-tests! *ns*)
