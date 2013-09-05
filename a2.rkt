#lang racket
;;;  Comp3007 Assignment 2
;;;
;;;  Frederic Marchand
;;;  ID# 100817579




;;;  Question 1   
(display "Question 1")
(newline)


;; make interval procedure
(define (make-interval begin end)
  (cons begin end)); (being . end)

;;procedure to access the lower bound of an interval
(define (lower-bound interval)
  (car interval));returns the first value of the interval

;;procedure to access the upper-bound of an interval
(define (upper-bound interval)
  (cdr interval));returns the last value of the interval

;;add intervals procedure
;Addition: [a,b] + [c,d] = [a+c, b+d]
(define (add-interval interval1 interval2)
  (make-interval (+ (lower-bound interval1) (lower-bound interval2))
                 (+ (upper-bound interval1) (upper-bound interval2)))
  )

;; substract interval procedure
;Subtract: [a,b] - [c,d] = [a-d, b-c]
(define (subtract-interval interval1 interval2)
  (make-interval (- (lower-bound interval1) (upper-bound interval2))
                 (- (upper-bound interval1) (lower-bound interval2)))
  )

;; multiply interval procedure
;Multiply: [a,b] * [c,d] = [min(ac,ad,bc,bd),max(ac,ad,bc,bd)]
(define (multiply-interval interval1 interval2)
  (make-interval (min (* (lower-bound interval1) (lower-bound interval2)) (* (lower-bound interval1) (upper-bound interval2)) (* (upper-bound interval1) (lower-bound interval2)) (* (upper-bound interval1) (upper-bound interval2)))
                 (max (* (lower-bound interval1) (lower-bound interval2)) (* (lower-bound interval1) (upper-bound interval2)) (* (upper-bound interval1) (lower-bound interval2)) (* (upper-bound interval1) (upper-bound interval2)))
                 ))

;; divide interval procedure
;Divide: [a,b] / [c,d] = [a,b] * [1/d, 1/c] if [c,d] does not contain 0
(define (divide-interval interval1 interval2)
  (cond ((or (= (lower-bound interval2) 0) (= (upper-bound interval2) 0))
         "Invalid operation")
        (else
         (multiply-interval interval1 (make-interval (/ 1 (upper-bound interval2)) (/ 1 (lower-bound interval2))))))
  )

;;test cases
(make-interval 1 5);returns the interval (1 . 5)

(add-interval (make-interval 1 5) (make-interval 4 8)) ;value should be (5 . 13)
(subtract-interval (make-interval 1 5) (make-interval 4 8)) ;value should be (-7 . 1)
(multiply-interval (make-interval 1 5) (make-interval 4 8)) ;value should be (4 . 40)
(divide-interval (make-interval 1 5) (make-interval 4 8)) ;value should be (1/8 . 5/4) or (1/8 . 1 1/4)
(divide-interval (make-interval 1 5) (make-interval 0 8)) ;should be invalid operation
(divide-interval (make-interval 1 5) (make-interval 0 0)) ;should be invalid operation
(divide-interval (make-interval 1 5) (make-interval 5 0)) ;should be invalid operation
(divide-interval (make-interval 1 5) (make-interval -8 10)) ;value should be (-5/8 . 1/2)



;;; -----------------------------------------------------------------------------------------------------


;;;  Question 2  
(newline)
(display "Question 2")
(newline)
(display "a)")
(newline)

;;a) 
;constructor for a record creates a list in the following format: ((( Yellow Submarine) 1968 (The Beatles) Apple))
(define (make-record title year artist label)
  (list (list (list title) year (list artist) label))
  )

;;get title procedure
(define (record-title record) (car (car (car record)))) ;gets the caaar of the record

;get artist procedure
(define (record-artist record) (car (car (cdr (cdr (car record)))))) ;gets the caaddar of the record

;get label procedure
(define (record-label record) (car (cdr (cdr (cdr (car record)))))) ;gets the cadddar of the record


;;TESTING
;define 5 records for test cases
(define aRecord (make-record "Yellow Submarine" 1968 "The Beatles" "Apple"))
(define bRecord (make-record "Thriller" 1982 "Michael Jackson" "Epic Records"))
(define cRecord (make-record "The Dark Side of the Moon" 1973 "Pink Floyd" "Capitol Records"))
(define dRecord (make-record "Let It Bleed" 1969 "The Rolling Stones" "Decca Records"))
(define eRecord (make-record "Abbey Road" 1969 "The Beatles" "Apple"))
(define allrecords (list aRecord bRecord cRecord dRecord eRecord));create a list of all the records
;print each record
aRecord
bRecord
cRecord
dRecord
eRecord
;;tests the title getter procedure
(record-title aRecord) ;should return "yellow submarine"
;tests the artist getter procedure
(record-artist aRecord) ;should return "The Beatles"
;tests the label getter procedure
(record-label aRecord) ;should return "Apple"


;;b) 
(newline)
(display "b)")
(newline)

;;adds a record to alist
;the function goes through the entire list and creates a new one containing all the values of the original list 
;with the new record appended to it
(define (add-record-to-list record alist)
  (if (null? alist) 
      (list record)
      (cons (car alist) 
            (add-record-to-list record (cdr alist))))) 


(add-record-to-list eRecord (add-record-to-list dRecord (list aRecord bRecord cRecord)));;append to list
(newline)
(add-record-to-list dRecord '());;empty list case


;;c) 
(newline)
(display "c)")
(newline)

(define delete-from-record-list
  (lambda (title alist)
    (cond
      ((null? alist) "Invalid Operation");checks if the list is empty
      ((equal? title (record-title (car alist))) (cdr alist)) ;if the title of the record matches the one we are looking for skip it and returns the rest of the list
      (else (cons (car alist) (delete-from-record-list title (cdr alist))))))) ;otherwise cons the first item of the list to the rest of the list with the deleted item (recursive step)

;;TEST CASES 
;these cases delete every record in a list of 5 records separatly showing that deletion works for any index in the list
(delete-from-record-list "Thriller" allrecords); returns the list without the record "Thriller"
(newline)
(delete-from-record-list "Yellow Submarine" allrecords); returns the list without the record "Yellow Submarine"
(newline)
(delete-from-record-list "The Dark Side of the Moon" allrecords); returns the list without the record "The Dark Side of the Moon"
(newline)
(delete-from-record-list "Let It Bleed" allrecords); returns the list without the record "Let It Bleed"
(newline)
(delete-from-record-list "Abbey Road" allrecords); returns the list without the record "Abbey Road"
(newline)
;this test deletion from an empty list
(delete-from-record-list "Thriller" '()) 


;;d) 
(newline)
(display "d)")
(newline)

;;this creates a list containing only the artist passed as parameter
(define (list-by-artist artist lst)
  (cond ((null? lst) '());; if the list is null return the empty list
        ((equal? artist (record-artist(car lst)));;if the artist is the right one add it to the output list
         (cons (car lst)
               (list-by-artist artist (cdr lst))))
        (else (list-by-artist artist (cdr lst)))));otherwise filter the rest of the list


(list-by-artist "The Beatles" (list aRecord bRecord cRecord dRecord eRecord));;returns a list containing both beatles records
(list-by-artist "Pink Floyd" (list aRecord bRecord cRecord dRecord eRecord));;returns a list containing Pink Floyds record
(list-by-artist "The Beatles" '());returns the empty list




;;; -----------------------------------------------------------------------------------------------------


;;;  Question 3  
(newline)
(display "Question 3")
(newline)

;;a) 
(newline)
(display "a)")
(newline)

;;implementation of special-cons from the assignment
(define (special-cons x y)
  (lambda (m) (m x y)))

;;implementation of special-car
(define (special-car value)
  (value (lambda (x y) x)))

;;implementation of special-cdr
(define (special-cdr value)
  (value (lambda (x y) y)))

;;test cases
(define special (special-cons 1 2))
special ;;this is a procedure
(special-car special);; returns 1
(special-cdr special);; returns 2


;;b)
(newline)
(display "b)")
(newline)
;;alternate implentations of cons returns a list
(define (alternate-cons x y)
   (list x y))

;;alternate implementation of car returns the element at index 0 of the list
(define (alternate-car value)
      (list-ref value 0))

;;alternate implementation of cdr returns the element at index 1 of the list
(define (alternate-cdr value)
      (list-ref value 1))

;test cases
(define alternate (alternate-cons 1 2))
alternate;; outputs '(1 2)
(alternate-car alternate);;returns 1
(alternate-cdr alternate);;returns 2

;;c) 
(newline)
(display "c)")
(newline)

;;triple implementation
(define (triple x y z)
  (lambda (m) (m x y z)))

;;first implementation returns the first element in the triplet
(define (first value)
  (value (lambda (p q r) p)))

;;second implementation returns the second element in the triplet
(define (second value)
  (value (lambda (p q r) q)))

;;third implementation returns the third element in the triplet
(define (third value)
  (value (lambda (p q r) r)))

(define triplet (triple 1 2 3))
triplet
(first triplet);;returns 1
(second triplet);;returns 2
(third triplet);;returns 3



;;; -----------------------------------------------------------------------------------------------------


;;;  Question 4   
(newline)
(display "Question 4")
(newline)

;;a) 
(newline)
(display "a)")
(newline)

;;this functions takes a list of elements and ouputs the powerset of that set
(define (subsets lis)
  (if (null? lis) '(())
      (let ((rest (subsets (cdr lis))))
        (append rest (map 
                      (lambda (subset)
                        (cons (car lis) subset)) rest)))))

;;test cases from empty set to multiple elements, they return the power set of each set
(subsets '());;outputs the the empty set: '(())
(subsets '(blah));;outputs: '(() (blah))
(subsets '(a b c d));; outputs: '(() (d) (c) (c d) (b) (b d) (b c) (b c d) (a) (a d) (a c) (a c d) (a b) (a b d) (a b c) (a b c d))
(subsets '(1 2 3 4));; outputs: '(() (4) (3) (3 4) (2) (2 4) (2 3) (2 3 4) (1) (1 4) (1 3) (1 3 4) (1 2) (1 2 4) (1 2 3) (1 2 3 4))
(subsets '(john jim bob)); outputs: '(() (bob) (jim) (jim bob) (john) (john bob) (john jim) (john jim bob))



;;b) 
(newline)
(display "b)")
(newline)

;;iterative filter procedure returns a list containing only the elements that return true to the predicate
(define (filter-iter pred lst)
  
  (define (iter pred lst newlst)
    (cond ((null? lst) newlst);; return the empty list if the list is null
          ((pred (car lst));;if the first element of the list passes the test
           (iter pred (cdr lst) (cons (car lst) newlst)));tail recursive step (if the first value passes the predicate test add it to the output list
          (else (iter pred (cdr lst) newlst))));;otherwise continue on with the rest of the lsit
  
  (reverse (iter pred lst '())))
        ;(else (filter-iter pred (cdr lst)))))

;test cases
(filter-iter even? '(1 2 3 4 5 6)) ;;(2 4 6) returns even numbers
(filter-iter odd? '(1 2 3 4 5 6))  ;;(1 3 5) returns odd numbers
(filter-iter pair? '((1 2) b (3 4) 5 7 (1 2 3)))  ;;((1 2) (3 4) (1 2 3)) ;;returns pairs only


;;c) 
(newline)
(display "c)")
(newline)

;;interleave procedure
(define (interleave lst1 lst2)
  (cond ((null? lst1) lst2);;if the first list is null return the second one
        ((null? lst2) lst1);; if the seocnd list is null return the first one
        ;otherwise append the first element of the first list to the first element of the second list and append the rest of the interleave list recursively
        (else (append (list (car lst1)) (list (car lst2)) (interleave (cdr lst1) (cdr lst2))))))

(interleave '(1 2 3 4) '(a b c d));;returns '(1 a 2 b 3 c 4 d)
(interleave '(a b c) '(d e f g));; returns '(a d b e c f g)
(interleave '() '(d e f g));;returns '(d e f g)
(interleave '(1 2 3 4) '());;returns '(1 2 3 4)
(interleave '() '());;returns '()


;;d) 
(newline)
(display "d)")
(newline)

;;returns the mean of the numbers contained in the list
(define (arith-mean lst)
  
  ;;returns the sum of all the numbers in the list
  (define (sum-numbers lst sum)
    (cond ((null? lst) sum)
          ((number? (car lst)) 
           (sum-numbers (cdr lst) (+ sum (car lst))))
          (else 
           (sum-numbers (cdr lst) sum)))) 
  
  ;;returns the amount of numbers contained in the list
  (define (number-count lst count)
    (cond ((null? lst) count)
          ((number? (car lst)) 
           (number-count (cdr lst) (+ count 1)))
          (else 
           (number-count (cdr lst) count))
          ))
  
  ;;performs the mean of all the numbers in the list
  (define (my-mean lis)
    (let ((sum (sum-numbers lis 0)) (count (number-count lis 0)))
      (cond ((not (= count 0)) (/ sum count))
            (else 0))))
  
  (my-mean lst))



;test cases
(arith-mean '(1 2 3 4 5));;mean is 3
(arith-mean '(1 a 2 b c d e 3));; mean is 2
(arith-mean '(a b c d e));;mean is 0
(arith-mean '());; mean is 0



;;; -----------------------------------------------------------------------------------------------------


;Question 5  
(newline)
(display "Question 5")
(newline)

;make-graph local state
;;the graph is represented as a list containing edges in the format '(a . b) where a and b are the values of the nodes and a single node is in the format '(a) where a is the value
;;a sample graph with multiple items can look like this: '((1 . 2) (5) (4 . 7) (2 . 7))
(define make-graph
  
   (lambda (graph)
     
     ;;deletes a single node that isnt attached to anything
     (define (delete-single-node item the-graph)
         (cond
           ((null? the-graph) '())
           ((equal? item (car the-graph)) (cdr the-graph))
           (else (cons (car the-graph) (delete-single-node item (cdr the-graph))))))
     
     ;;deletes a node from the graph
     (define (delete-node item the-graph)
         (cond
           ((null? the-graph) '())
           ((equal? (car item) (car (car the-graph))) (delete-node item (cdr the-graph)))
           ((equal? (car item) (cdr (car the-graph))) (delete-node item (cdr the-graph)))
           (else (cons (car the-graph) (delete-node item (cdr the-graph))))))
     
     ;;deletes an edge from the graph
     (define (delete-edge item the-graph)
         (cond
           ((null? the-graph) '())
           ((equal? item (car the-graph)) (delete-edge item (cdr the-graph)))
           ((equal? (cons (cdr item) (car item)) (car the-graph)) (delete-edge item (cdr the-graph)))
           (else (cons (car the-graph) (delete-edge item (cdr the-graph))))))
     
     ;;checks if a the given node is contained in the graph
     (define (contains-node? the-graph item)
       (if (empty? the-graph) #f
           (or (equal? (car (car the-graph)) item) (equal? (cdr (car the-graph)) item) (contains-node? (cdr the-graph) item))))
     
     ;;checks if a given edge is contained in the graph
     (define (contains-edge? the-graph item)
       (if (empty? the-graph) #f
           (or (equal? (car the-graph) item) (contains-edge? (cdr the-graph) item))))
       
     ;;adds a node to the given graph
     (define (add-a-node item the-graph)
       (cond ((not (contains-node? the-graph (car item))) (cons item the-graph))
             (else the-graph)))
     
     ;;adds an edge to the given graph
     (define (add-an-edge item the-graph)
       (cond ((and (not (contains-edge? the-graph item)) (not (contains-edge? the-graph (cons (cdr item) (car item))))) (cons item the-graph))
             (else the-graph)))
     
     
     (lambda (command . args)
       
       (case command
         ;;adds a node to the graph
         ((add-node) (set! graph (add-a-node (car args) graph)))
         ;adds an edge to the graph
         ((add-edge) (set! graph (delete-single-node (list (cdr (car args)))(delete-single-node (list (car (car args))) (add-an-edge (car args) graph)))))
                   
         ;;deletes a node from the graph
         ((delete-node) (set! graph (delete-node (car args) graph)))
         ;deletes an edge from the graph
         ((delete-edge) (set! graph (delete-edge (car args) graph)))
         
         ;make-edge function
         ((make-edge) (cons (car args) (car (cdr args))))
         ;make-node function
         ((make-node) (list (car args)))
         
         ;prints the graph
         ((print-graph) graph) 
         (else 'error)))))

;make-edge function external definition
;(define (make-edge node1 node2) (cons node1 node2))

;make-node function external definition
;(define (make-node value) (list value))

;;TEST CASES
(define agraph (make-graph '()));;define a graph called agraph
agraph
(agraph 'make-edge 5 4);;displays the format of an edge '(5 . 4)
(agraph 'make-node 4);;displays the format of a single node '(4)
(newline)
(newline)

;;add an edge to the graph
(agraph 'add-edge (agraph 'make-edge 5 4))
(agraph 'print-graph);outputs '((5 . 4))

;;add the same edge that already exists
(agraph 'add-edge (agraph 'make-edge 5 4))
(agraph 'print-graph);doesnt work because its already in there

;;add a node to the graph
(agraph 'add-node (agraph 'make-node 5))
(agraph 'print-graph);;outputs '(5 . 4) because the node 5 already exists

;;add another node to the graph
(agraph 'add-node (agraph 'make-node 4))
(agraph 'print-graph);;doesnt work again because 4 is already a node part of an edge

;;add another node
(agraph 'add-node (agraph 'make-node 2))
(agraph 'print-graph);;outputs '((2) (5 . 4))

;;add the same node
(agraph 'add-node (agraph 'make-node 2))
(agraph 'print-graph);;doesnt work because we already added the node

;;add an edge
(agraph 'add-edge (agraph 'make-edge 2 7))
(agraph 'print-graph);;outputs '((2 . 7) (5 . 4))

;;add 3 more edges
(agraph 'add-edge (agraph 'make-edge 5 8))
(agraph 'add-edge (agraph 'make-edge 5 9))
(agraph 'add-edge (agraph 'make-edge 5 7))
(agraph 'print-graph);;outputs '((5 . 7) (5 . 9) (5 . 8) (2 . 7) (5 . 4))

;;delete node 5 should delete everything but '(2 . 7) because thats the only one not tied to node 5
(agraph 'delete-node (agraph 'make-node 5))
(agraph 'print-graph);as expected outputs '((2 . 7))

;;delete the remaining edge, the graph should be empty after this
(agraph 'delete-edge (agraph 'make-edge 2 7))
(agraph 'print-graph);outputs '() as expected

;add a node then delete it
(agraph 'add-node (agraph 'make-node 5))
(agraph 'print-graph);;outputs '((5))
(agraph 'delete-node (agraph 'make-node 5))
(agraph 'print-graph);;outputs '()

;;add an edge and delete it with the nodes in reverse order, the order of the nodes of an edge doesnt matter if the edge is there it gets deleted
(agraph 'add-edge (agraph 'make-edge 5 8));puts '(5 . 8)
(agraph 'print-graph);;ouputs '((5 . 8))
(agraph 'delete-edge (agraph 'make-edge 8 5));;tries to remove '(8 . 5) and succeeds
(agraph 'print-graph);;outputs '() which means the edge was successfully removed




;;; -----------------------------------------------------------------------------------------------------


;Question 6  
(newline)
(display "Question 6")
(newline)

;;a) 
(newline)
(display "a)")
(newline)

;;checks whether all the parameters passed in to the procedure are in increasing order
(define (increasing? first . rest)
  (cond ((null? rest) #t) ;;if the list is null return true
        ((> (car rest) first) ;;if the second element is greater than the first
         (apply increasing? rest)) ;;check whether the rest of the parameters are in increasing order
        (else #f)));;otherwise return false

;test cases
(increasing? 1 2 3 4);;returns true
(increasing? 1 2 4 6 5);;returns false
(increasing? 5 4 3 2);;returns false
(increasing? 5);;returns true
;this function takes at least 1 or more parameters


;;b) 
(newline)
(display "b)")
(newline)

;;outputs a list of all natural numbers in ascending order that are smaller than the number passed in
(define (iota-rec nat)
  (cond ((zero? nat) '());;if the number is zero return the empty list
        (else (append (iota-rec (- nat 1)) (list (- nat 1))))));;otherwuse append nat-1 to the rest of the recursively developed output list

;test cases
(iota-rec 4);; outputs '(0 1 2 3)
(iota-rec 5);; outputs '(0 1 2 3 4)
(iota-rec 6);; outputs '(0 1 2 3 4 5)
(iota-rec 0);; outputs '()
(iota-rec 1);; outputs '(0)


;;c)  
(newline)
(display "c)")
(newline)

;;outputs a list of all natural numbers in ascending order that are smaller than the number passed in iteratively
(define (iota-iter n)
  
  (define (iota n lis)
    (if (zero? n) ;; if the number is 0 
        lis ;return the list
        ;otherwise perform tail recursion on the n-1 natural numbers and append the n-1 number to the output list
        (iota (- n 1) (append (list (- n 1)) lis))));;tail recursion
  
  (iota n '()))

;;test cases , same results obtained as in b)
(iota-iter 4);; outputs '(0 1 2 3)
(iota-iter 5);; outputs '(0 1 2 3 4)
(iota-iter 6);; outputs '(0 1 2 3 4 5)
(iota-iter 0);; outputs '()
(iota-iter 1);; outputs '(0)


;;d)
(newline)
(display "d)")
(newline)

;;substitute all occurences of old by new in the given list
(define (subst old new lis)
  (cond ((null? lis) '());; if the list is null return the empty list
        ((equal? (car lis) old) (cons new (subst old new (cdr lis))));;if the first thing in the list is equal to the old character stick the new character to the rest of the substituted list
        (else (cons (car lis) (subst old new (cdr lis))))));; otherwise stick the first element to the rest of the rest of the substituted list

;test cases
(subst 'c 'k '(c o c o n u t));replace c by k, output is '(k o k o n u t)
(subst 'b 'q '(b a r b e c u e));replace b by q, output is '(q a r q e c u e)
(subst 'f 'p '(f e l a f e l));replace f by p, output is '(p e l a p e l)
(subst 'b 'g '(b b b b b b b b b));replace b by g; output is '(g g g g g g g g g)
(subst 'b 'g '());replace b by g , output is '()

;;; -----------------------------------------------------------------------------------------------------


;Question 7   
(newline)
(display "Question 7")
(newline)

;outputs a list containing all the combinations of size n 
(define (combinations n lis)
  (cond ((= n 0) '(()));; if we want combinations of size 0 return a nested empty list
        ((null? lis) '());; if the list is null return the empty lists
        ;otherwise append the map fo all combinations to the combinations of the rest of the list
        (else (append (map (lambda (decval)
                             (cons (car lis) decval))
                           (combinations (- n 1) (cdr lis)))
                      (combinations n (cdr lis))))))

;test cases
(combinations 5 '());;outputs '()
(combinations 0 '(joe jane jim sue));;outputs '(())
(combinations 2 '(joe jane jim sue));;outputs '((joe jane) (joe jim) (joe sue) (jane jim) (jane sue) (jim sue))
(combinations 3 '(a b c d e));;outputs '((a b c) (a b d) (a b e) (a c d) (a c e) (a d e) (b c d) (b c e) (b d e) (c d e))



;;; -----------------------------------------------------------------------------------------------------


;Question 8  
(newline)
(display "Question 8")
(newline)

;;outputs the maximum depth of a list
(define (depth lis)
  (cond ((null? lis) 0);if the list is null return 0
        ((not (pair? lis)) 0);; if the list is not a pair return 0
        ((list? (car lis));;if the car of the list is a list
         (max (+ 1 (depth (car lis))) (depth (cdr lis)))) ;return the max of the depth of the car of the lis + 1 and the depth of the cdr of the list
        ((symbol? (car lis)) (max 1 (depth (cdr lis))));;if the car of the lis is a symbol return the max of 1 and the depth of the cdr of the list
        (else 0)));otherwise return 0

;test cases
(depth '());; return 0
(depth 'a);return 0
(depth '(a));return 1
(depth '(a (b) c));return 2
(depth '(((((a(((b)))))))));returns 8



