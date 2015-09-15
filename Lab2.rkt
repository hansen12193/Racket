;Nathan Hansen
;CSCI301 Lab 2

#lang racket
;Decides if the element a is a member of the list L.
(define Member?
  (lambda (a L) 
    (cond ((null? L) #f)
          ((equal? a (car L)) #t)
          (else (Member? a (cdr L))))))
                  
;Decides if the list L contains duplicates and, if so, removes them.
(define Remove-Duplicates
  (lambda (L)
    (cond ((null? L) L)
          ((Member? (car L) (cdr L)) (Remove-Duplicates (cdr L)))
          (else (cons (car L) (Remove-Duplicates (cdr L)))))))

;Uses the Remove-Duplicates function to return the set-union of two sets.
(define Union
  (lambda (L1 L2)
    (Remove-Duplicates (append L1 L2))))

;Returns the intersection of two sets.
(define Intersection
  (lambda (L1 L2)
    (cond ((null? L1) L1)
          ((Member? (car L1) L2) (cons (car L1) (Intersection (cdr L1) L2)))
          (else (Intersection (cdr L1) L2)))))

;Returns the difference of the two sets.
(define Difference
  (lambda (L1 L2)
    (cond ((null? L1) L1)
          ((Member? (car L1) L2) (Difference (cdr L1) L2))
          (else (cons (car L1) (Difference (cdr L1) L2))))))

;Returns the symmetric diffrence of two sets.
(define Symmetric-Difference
  (lambda (L1 L2)
    (cond ((null? L1) (append L2 L1))
          ((Member? (car L1) L2) (Symmetric-Difference (cdr L1) (cdr L2)))
          (else (cons (car L1) (Symmetric-Difference (cdr L1) L2))))))
