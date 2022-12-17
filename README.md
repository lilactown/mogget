# 😼 mogget

A tiny stack-based concatenative language embedded in Clojure.

```clojure
1 2 + prn ; put two numbers on the stack, add them and print the result


;; use familar Clojure core functions, now in stack form

[1 2 3] ; put a vector on the stack
(inc) ; put a literal form on the stack that can be evaluated later
map ; eval the form on each element of the vector & put the result on the stack
prn ; print `(2 3 4)`


;; control flow

0 1 2 < (inc) (dec) if prn ; print `1`

0 1 2 < (inc) when prn ; print `1`

0 :foo {:foo (inc) :bar (dec)} case prn ; print `1`

0 1 2
((<) (inc)
 (>) (dec)) ? prn ; print `1`


;; manipulate the stack

0 5 range ; create a range of 0 to 5 numbers and put it on the stack
spread ; put each element of the range on the stack
3 vector ; take the top 3 elements from the stack and put them in a vector
swap ; swap the first two items of the stack: `0 [4 3 2] 1`
rm ; remove the first item off the stack, putting the vector back at the front
swap ; stack is now `0 [4 3 2]`
(+) ; put the list (+) on the stack
swap ; stack is now `[4 3 2] 0 (+)`
reduce ; reduce the vector using `0` as the initial value, evaluating `(+)`
prn ; print `8`


;; common combinators from Factor

;; `dup` duplicates an item on the stack
2 dup * prn ; print `4`

;; bi applies top two forms to the third item, pushing both values on the stack
2 (2 +) (dec) bi + prn ; print `5`

;; defining words

(sq (dup *)) ; push a list of a symbol and then the form to be eval'd
defn ; the defn word adds the symbol to the dictionary as a word
2 sq prn ; prints `4`
```
