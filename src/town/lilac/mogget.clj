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
  ([f & {:keys [arity results]
         :or {arity 1
              results 1}}]
   (fn [ctx]
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
            (conj stack (apply f args))
            ;; TODO throw exception if actual results > results
            (into stack (apply f args)))))))))

(declare eval-list)

(def default-words
  {'. display

   ;; arithmetic
   'inc (->fn inc)
   'dec (->fn dec)
   '+ (fn [{:keys [stack] :as ctx}]
        (let [[stack x] (-pop stack)
              [stack y] (-pop stack)]
          (assoc ctx :stack (conj stack (+ x y)))))
   '- (->fn - :arity 2)
   '* (->fn * :arity 2)
   '/ (->fn / :arity 2)

   ;; tests
   '< (->fn < :arity 2)
   '> (->fn > :arity 2)
   '= (->fn = :arity 2)

   ;; seqs
   'map (fn [{:keys [stack] :as ctx}]
          (let [[stack form] (-pop stack)
                [stack coll] (-pop stack)]
            (assoc
             ctx
             :stack
             (conj
              stack
              (map (fn [x]
                     (-> {:stack [x] :words default-words :mode :eval}
                         (eval-list form)
                         ;; TODO throw err if > 1 results on stack
                         :stack
                         first))
                   coll)))))

   ;; combinators
   'bi (->fn
        (fn [x p q]
          [(-> {:stack [x] :words default-words :mode :eval}
               (eval-list p)
               (:stack)
               ;; TODO throw err if > 1 results on stack
               (first))
           (-> {:stack [x] :words default-words :mode :eval}
               (eval-list q)
               (:stack)
               (first))])
        :arity 3 :results 2)

   ;; shuffle
   'swap (->fn (fn [x y] [y x]) :arity 2 :results 2)
   'dup (->fn (fn [x] [x x]) :arity 1 :results 2)})

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
  `(eval-list
    {:stack [] :words default-words :mode :eval}
    '~form))


(comment
  (eval 1 2 3 + +)
  (eval 2 1 -)
  (eval 1 inc)


  (eval [1 2 3] (inc) map)


  (eval 1 (inc) (inc) bi *)


  (eval 1 2 swap)
  (eval 1 dup)
  )
