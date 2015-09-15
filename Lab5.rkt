;Nathan Hansen
;CSCI301 Lab 2
#lang racket

(define set-value
  (lambda (L1)
    (cond ((equal? (car L1)) 'complement) (complement (car L1)))
          ((equal? (car (cdr L1)) 'difference) (difference (flatten (car L1)) (flatten (cdr (cdr L1)))))
          ((equal? (car (cdr L1)) 'symmetric-difference) (symmetric-difference (car L1) (cdr (cdr L1))))
          ((equal? (car (cdr L1)) 'intersection) (intersection (flatten (car L1)) (flatten (cdr (cdr L1)))))
          ((equal? (car (cdr L1)) 'complement) (complement (car L1)))))
         

;Decides if the element a is a member of the list L.
(define member?
  (lambda (a L) 
    (cond ((null? L) #f)
          ((equal? a (car L)) #t)
          (else (member? a (cdr L))))))

;Returns the intersection of two sets.
(define intersection
  (lambda (L1 L2)
    (cond ((null? L1) L1)
          ((member? (car L1) L2) (cons (car L1) (intersection (cdr L1) L2)))
          (else (intersection (cdr L1) L2)))))

;Return the complement of the two sets.
(define complement
  (lambda (L1)
    (difference '(a b c d e f g h i j k l m) '(L1))))

;Returns the difference of the two sets.
(define difference
  (lambda (L1 L2)
    (cond ((null? L1) L1)
          ((member? (car L1) L2) (difference (cdr L1) L2))
          (else (cons (car L1) (difference (cdr L1) L2))))))

;Returns the symmetric diffrence of two sets.
(define symmetric-difference
  (lambda (L1 L2)
    (cond ((null? L1) (append L2 L1))
          ((member? (car L1) L2) (symmetric-difference (cdr L1) (cdr L2)))
          (else (cons (car L1) (symmetric-difference (cdr L1) L2))))))
 
;Return the union of two sets.
(define union
  (lambda (L1 L2)
    (cond ((null? L2) L1)
          ((member? (car L2) L1)
          (union L1 (cdr L2)))
          (#t (union (cons (car L2) L1) (cdr L2))))))
  

