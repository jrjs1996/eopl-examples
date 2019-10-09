#lang eopl


;; duple : int  any -> ListOf(any)
;; usage: Takes a value X and int n and returns a list of length n with each value being X.
(define duple
  (lambda (n x)
    (if (eqv? n 0)
        '()
        (cons x (duple (- n 1) x)))))

;; 1.16
;; invert : lst -> lst
;; Takes a list of list of 2 lists and returns a list with each 2-List reversed
(define invert
  (lambda (lst)
    (if (eqv? lst '())
        lst
        (let ([2lst (car lst)])
          (cons (cons (cadr 2lst) (car 2lst)) (invert (cdr lst)))))))

;; 1.17
;; down : lst -> lst
;; wraps parentheses around each top-level element of
;; lst.
(define down
  (lambda (lst)
    (if (eqv? lst '())
        lst
        (cons (cons (car lst) '()) (down (cdr lst))))))

;; 1.18
;; swapper : s1 s2 slist -> slist
;; returns a list the same as slist, but
;; with all occurrences of s1 replaced by s2 and all occurrences of s2 replaced by s1.
(define swapper-sexp
  (lambda (s1 s2 sexp)
    (if (symbol? sexp)
        (if (eqv? sexp s1)
            s2
            (if (eqv? sexp s2)
                s1
                sexp))
        (swapper s1 s2 sexp))))

(define swapper
  (lambda (s1 s2 slist)
    (if (null? slist)
        '()
        (cons (swapper-sexp s1 s2 (car slist)) (swapper s1 s2 (cdr slist))))))

;; 1.19
;; list-set : lst n x -> lst
;; (list-set lst n x) returns a list like lst, except that the n-th
;; element, using zero-based indexing, is x.
(define list-set
  (lambda (lst n x)
    (if (eqv? n 0)
        (cons x (cdr lst))
        (cons (car lst) (list-set (cdr lst) (- n 1) x)))))

;; 1.20
;; count-occurrences : s slist -> n
;; the number of occurrences of s in slist.
(define count-occurrences-sexp
  (lambda (s sexp)
    (if (symbol? sexp)
        (if (eqv? s sexp)
            1
            0)
        (count-occurrences s sexp))))

(define count-occurrences
  (lambda (s slist)
    (if (null? slist)
        0
        (+ (count-occurrences-sexp s (car slist)) (count-occurrences s (cdr slist))))))

;; 1.21
;; product : sos1 sos2 -> los
;; where sos1 and sos2 are each a list
;; of symbols without repetitions, returns a list of 2-lists that represents the Cartesian
;; product of sos1 and sos2. The 2-lists may appear in any order.
(define product-internal
  (lambda (sos1 copysos2 sos2)
    (if (null? sos1)
        '()
        (if (null? sos2)
            (product-internal (cdr sos1) copysos2 copysos2)
            (cons (cons (car sos1) (car sos2)) (product-internal sos1 copysos2 (cdr sos2)))))))

(define product
  (lambda (sos1 sos2)
    (product-internal sos1 sos2 sos2)))

;; 1.22
;; filter-in : pred lst -> lst
;; returns the list of those elements in
;; lst that satisfy the predicate pred.
(define filter-in
  (lambda (pred lst)
    (if (null? lst)
        '()
        (let ([cur (car lst)])
          (if (pred cur)
              (cons cur (filter-in pred (cdr lst)))
              (filter-in pred (cdr lst)))))))


;; 1.23
;; list-index : pred lst -> int
;; returns the 0-based position of the
;; first element of lst that satisfies the predicate pred. If no element of lst satisfies
;; the predicate, then list-index returns #f.
(define list-index-internal
  (lambda (pred lst index)
    (if (null? lst)
        #f
        (if (pred (car lst))
            index
            (list-index-internal pred (cdr lst) (+ index 1))))))

(define list-index
  (lambda (pred lst)
    (list-index-internal pred lst 0)))
;; 1.24
;; every? pred lst -> bool
;; returns #f if any element of lst fails to
;; satisfy pred, and returns #t otherwise.
(define every?
  (lambda (pred lst)
    (if (null? lst)
        #t
        (if (pred (car lst))
            (every? pred (cdr lst))
            #f))))

;; 1.25
;; exists? : pred lst -> bool
;; returns #t if any element of lst satisfies
;; pred, and returns #f otherwise.
(define exists?
  (lambda (pred lst)
    (if (null? lst)
        #f
        (if (pred (car lst))
            #t
            (exists? pred (cdr lst))))))

;; 1.26
;; up : lst -> lst
;; removes a pair of parentheses from each top-level element
;; of lst. If a top-level element is not a list, it is included in the result, as is.
;; The value of (up (down lst)) is equivalent to lst, but (down (up lst)) is
;; not necessarily lst. (See exercise 1.17.)
(define up-sublist
  (lambda (sublist lst)
    (if (null? sublist)
        (up lst)
        (cons (car sublist) (up-sublist (cdr sublist) lst)))))
        

(define up
  (lambda (lst)
    (if (null? lst)
        '()
        (let ([cur (car lst)])
          (if (list? cur)
              (up-sublist cur (cdr lst))
              (cons cur (up (cdr lst))))))))


;; 1.28
;; flatten : slist -> list
;; returns a list of the symbols contained in
;; slist in the order in which they occur when slist is printed. Intuitively, flatten
;; removes all the inner parentheses from its argument.
(define flatten-internal
  (lambda (sublist lst)
    (if (null? sublist)
        (if (null? lst)
            '()
            (flatten-internal (car lst) (cdr lst)))
        (let ([cur (car sublist)][rest (cdr sublist)])
          (if (list? cur)
              (flatten-internal cur (cons rest lst)) 
              (cons cur (flatten-internal rest lst)))))))

(define flatten
  (lambda (slist)
    (flatten-internal slist '())))

;; 1.29

;; 1.30

;; 1.31

;; 1.32

;; 1.33

;; 1.34

;; 1.35

;; 1.36









