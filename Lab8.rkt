;Nathan Hansen
;CSCI301 Lab 8
#lang racket

;Determines if two sets are equal.
(define set-equal? 
  (lambda (L1 L2) 
    (and (subset? L1 L2) (subset? L2 L1))))

;Determines if a is a member of L.
(define member?
  (lambda (a L) 
    (cond ((null? L) #f)
          ((and (and (not (list? a)) (not (list? (car L)))) 
             (equal? a (car L))) #t)
          ((and (not (and (not (list? a)) (not (list? (car L))))) 
                (element-equal? (car a) (car L)) #t))
          (else (member? a (cdr L))))))

;Determines if two lists are subsets of each other.
(define subset? 
  (lambda (L1 L2)
    (or (null? L1)
        (and (member? (car L1) L2) (subset? (cdr L1) L2)))))

;Determines if two lists are equal.
(define element-equal?
  (lambda (L1 L2)
    (cond ((null? L1) #t)
          ((equal? (list->set L1) (list->set L2)) #t)
          (else #f))))