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
  {'. display

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
   'reduce (->fn (fn [words coll init f]
                   (reduce (fn [res x]
                             (-> {:stack [res x] :words words :mode :eval}
                                 (eval-list f)
                                 :stack
                                 ;; TODO throw if > 1 result
                                 first))
                           init
                           coll)) :arity 3)

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

   ;; shuffle
   'swap (->sfn (fn [x y] [y x]) :arity 2 :results 2)
   'dup (->sfn (fn [x] [x x]) :arity 1 :results 2)

   'define (fn [{:keys [stack words] :as ctx}]
             (let [[stack form] (-pop stack)
                   [stack quoted-sym] (-pop stack)
                   sym (second quoted-sym)]
               (assoc
                ctx
                :stack stack
                :words (assoc words sym
                              (fn [ctx]
                                (eval-list ctx form))))))})

(defprotocol IEval
  (-eval [x stack]))

(extend-protocol IEval
  Object
  (-eval [o ctx] (update ctx :stack conj o))

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

(defn eval-list
  [ctx list]
  (reduce
    (fn [ctx x]
      (-eval x ctx))
    ctx
    list))

(defmacro eval
  [& form]
  `(:stack
    (eval-list
     {:stack [] :words default-words :mode :eval}
     '~form)))


(comment
  ;; arithemetic
  (eval 1 2 3 + +)
  (eval 2 1 -)
  (eval 1 inc)

  ;; testing
  (eval 1 1 =)
  (eval [1 2 3] [1 2 3] =)
  (eval 1 2 =)

  (eval 1 2 <)
  (eval 2 1 <)

  (eval 2 1 >)
  (eval 1 2 >)

  ;; seq
  (eval [1 2 3] first)
  (eval [1 2 3] second)
  (eval [1 2 3] 2 nth)
  (eval [1 2 3] 4 conj)
  (eval [1 2 3] (inc) map)
  (eval [1 2 3] 0 (+) reduce)

  ;; combinator
  (eval 1 (inc) (inc) bi *)

  ;; shuffle
  (eval 1 2 swap)
  (eval 1 dup)

  ;; define
  (eval 'sq (dup *) define 2 sq)
  (eval 'sq (dup *) define [1 2 3] (sq) map)
  (eval 'sq (dup *) define
        3 (sq) (sq) bi)
  )
