;Nathan Hansen
;CSCI301 Lab 4
#lang racket

;Define a function that will extract only the prop variables and leave out the statements.
(define collect-prop-variables
  (lambda (L)
    (cond ((null? L) L)
          ((list? (car L)) (collect-prop-variables (car L)))
          ((equal? (car L) 'or) (collect-prop-variables (cdr L)))
          ((equal? (car L) 'and) (collect-prop-variables (cdr L)))
          ((equal? (car L) 'not) (collect-prop-variables (cdr L)))
          ((equal? (car L) 'implies) (collect-prop-variables (cdr L)))
          ((equal? (car L) 'iff) (collect-prop-variables (cdr L)))
          (else (remove-duplicates (cons (car L) (collect-prop-variables (cdr L))))))))

;Helper for collect-prop-variables that removes dulpicate elements of the final list.
(define remove-duplicates
  (lambda (L)
    (cond ((null? L) L)
          ((member? (car L) (cdr L)) (remove-duplicates (cdr L)))
          (else (cons (car L) (remove-duplicates (cdr L)))))))

;Helper for remove-duplicates that will decide if an element is a member of a list.
(define member?
  (lambda (a L) 
    (cond ((null? L) #f)
          ((equal? a (car L)) #t)
          (else (member? a (cdr L))))))

;-------------------------------------------------------------------------------------------

;Define a function that will replace all instances of a given variable in a truth expression with a given element.
(define substitute
  (lambda (L v e)
    (cond ((null? L) L)
          ((list? (car L)) (cons (substitute (car L) v e) (substitute (cdr L) v e)))
          ((equal? (car L) v) (cons e (substitute (cdr L) v e)))
          (else (cons (car L) (substitute (cdr L) v e))))))
