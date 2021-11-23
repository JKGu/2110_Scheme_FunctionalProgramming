;; (byTwos n m) returns the list of every other integer starting with n up to m.
;; Base Case: if n > m, the result is the empty list.
;; Hypothesis: Assume (byTwos (+ n 2) m) returns the list of every other integer
;; from n+2 up to m.
;; Recursive step: (byTwos n m) returns (cons n (byTwos (+ n 2) m))

(define (byTwos n m)
  (cond ((<= n m) (cons n (byTwos (+ n 2) m)))
        (else '())
   )
)

;; (compress L) returns a list of all the atoms
;; Base Case: if L is null, return it
;; Hypothesis: Assume (compress (car L)) and (compress (cdr L)) returns correctly
;; Recursive step: if first element of L is a list, return the concatenated list of (compress (car L)) and (compress (cdr L))
;; if first element of L is an atom, add it to (compress (cdr L))
(define (compress L)
  (cond   ((null? L) L)
          ((list? (car L)) (append (compress(car L)) (compress(cdr L)) ) )
          (else (cons (car L) (compress (cdr L))))
 ))


;; (rev-all L) recursively reverses a list L
;; base case: if old is null, return new
;; hypothesis: assume (rev (cdr old) (cons (rev (car old) '()) new)  ) and (rev (cdr old) (cons (car old) new)) are correct
;; Recursive step: if first element of old is list, call rev on that element as well as the rest of the list
;; if first element of old is atom, call rev on the rest of list and add that atom to new list
(define (rev-all L)
  (rev L '())
  )
(define (rev old new)
  (cond ((null? old) new)
        ((list? (car old))   (rev (cdr old) (cons (rev (car old) '()) new)  )   )
        (else (rev (cdr old) (cons (car old) new))  )
  ))

;; (equalTo? x y) compares x and y
;; base case: if x and y are both null or empty lists, or equal atoms, return True
;; hypothesis: assume (equalTo? (car x) (car y) (equalTo?  (cdr x) (cdr y)) works correctly
;; recursive step: if x and y are lists, (equalTo? x y) will be and result of (equalTo? (car x) (car y) (equalTo?  (cdr x) (cdr y))
;;
(define (equalTo? x y)
 (cond
   ((and (list? x)(list? y))
    (cond
      ((and (null? x) (null? y))  #t  )
      ((= (length x)(length y)) (and (equalTo? (car x) (car y)) (equalTo?  (cdr x) (cdr y)) )    )
      (else #f)
    ))
   (else  (eq? x y))
   )
  )


;;(equalFns? fn1 fn2 domain), where the parameters fn1 and fn2 are functions and domain is a list of values, that returns true iff fn1 and fn2 always returns the same value when applied to the same element of domain.
;; base case: if domain is empty, result is trivally true
;; assumption: assume that equalFns? works correctly with (cdr domain)
;; recursive: the result for (cdr domain) is that, if first element in domain results equally with fn1 and fn2, it will depend on whether (cdr domain) is true. if 1st ele is different, return false.

(define (equalFns? fn1 fn2 domain)
  (cond
    ((null? domain) #t)
    ((equal? (fn1 (car domain))  (fn2 (car domain)) )   (equalFns? fn1 fn2 (cdr domain))    )
    (else #f)
    )
)
 

;; (same-vals fn1 fn2 domain), that returns the list of all elements x of domain such that (fn1 x) and (fn2 x) return the same value
;; base case: when domain is empty, the result is trivally an empty list
;; assumption: assume that same-vals correctly works on sublist (cdr domain)
;; recursive step: if the first element results equally from fn1& fn2, add that element to our return list and examine the rest of the domain. If not, ignore that and continue with the rest of the domain list.

(define (same-vals fn1 fn2 domain)
  (cond
    ((null? domain) domain)
    ((equal? (fn1 (car domain))  (fn2 (car domain)) )         (cons (car domain)  (same-vals fn1 fn2 (cdr domain))   )              )
    (else   (same-vals fn1 fn2 (cdr domain))   )
    )
  )


;;(split x L), where x is a number and L is a list of numbers, that returns a list containing two lists: The first list contains the numbers in L less than or equal to x and the second list contains the numbers in L greater than x. 
;; base: when L is empty, return '(()())
;; assumption: assume split works correctly with (cdr L)
;; recursive: if 1st ele <= x, add it to first list in (split (cdr L)). Add to 2nd list otherwise.
(define (split x L)
(cond
  ((null? L) '(()()))
  ((<= (car L) x)
   (list
    (cons (car L) (car (split x (cdr L)) ) )
    (car(cdr( split x (cdr L)) ))
    ))
  (else
   (list  
          (car (split x (cdr L)))
          (cons (car L) (car(cdr (split x (cdr L)) )) )
   )
  )))


;; (psort L) that implements a partition sort (similar to Quicksort).
;; base case: when L is empty, return it
;; assumption: assume that psort correctly works on sublists x and y, where all ele in x <= a pivot and all ele in y > pivot
;; recursive: split the L and combine the results
(define (psort L)
  (cond
    ((null? L)  L   )
    (else         (let ((x  (split (car L)  (cdr L))))
                    (append (psort (car x)) (cons (car L) (psort (cadr x))))
                    )
     )
 ))


;;(applyToList f), where f is a parameter that is a function, that returns a function that takes a list L as a parameter and applies f to every element of L, returning the resulting list as the result. 
(define (applyToList f)
  (lambda (L) (map f L))
  )



(define (newApplyToList f)
  (lambda (L)
      (letrec  ((func (lambda (x)
                        (cond ((null? x) x) (else (cons (f (car x)) (func (cdr x))))   )
                        )
                      ))
        (func L)
          )
      )
)

(byTwos 1 20)

(compress '(1 (2 3 (4 5) (6 7 (8)) 9) 10))

(rev-all '(1 2 (3 4) (5 6 (7 8) 9) 10) )

(equalTo? '(1 2 3) '(1 2 3))
 (equalTo? 3 4)

(equalFns? (lambda (x) (* x 2)) (lambda (y) (+ y y))
'(1 2 3 4 5 6 7 8 9 10 11 12))
 (equalFns? (lambda (x) (* x 2)) (lambda (y) (+ y 2)) '(2))
(equalFns? (lambda (x) (* x 2)) (lambda (y) (+ y 2)) '(2 3 4 5))
 (equalFns? (lambda (L) (car L)) (lambda (L) (cadr L))
'(((2 3) (2 3)) ((4 5) (4 5))))

(same-vals (lambda (x) x)
(lambda (y) (abs y)) ;; abs give the absolute value
'(-3 -2 -1 0 1 2 3))

(split 6 '( 1 9 2 8 3 10 4 6 5))
(split 7 '(1 9 2 8 3 10 4 6 5))

(psort '(5 3 8 6 1 0 2))

(define g (applyToList (lambda (x) (* x 2))))
(g '(1 2 3 4 5))
((applyToList (lambda (y) (car y))) '((1 2 3) (4 5 6) (7 8 9)))


(define k (newApplyToList (lambda (x) (* x 2))))
(k '(1 2 3 4 5))
((newApplyToList (lambda (y) (car y))) '((1 2 3) (4 5 6) (7 8 9)))