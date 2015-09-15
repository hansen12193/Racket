;Nathan Hansen
;CSCI301 Lab 7
#lang racket

;Helper function that builds a list consisting of the second binary elements.
(define Builder
  (lambda (L)
    (cond ((null? L) L)
          (else (cons (car (cdr (car L))) (Builder (cdr L)))))))

;-----------------------------------------------------------------------------------

;Helper function that will decide if an element is a member of a list.
(define member?
  (lambda (a L) 
    (cond ((null? L) #f)
          ((equal? a (car L)) #t)
          (else (member? a (cdr L))))))

;-----------------------------------------------------------------------------------

;Helper function that builds a list consisting of the first binary elements.
(define Builder2
  (lambda (L)
    (cond ((null? L) L)
          (else (cons (car (car L)) (Builder2 (cdr L)))))))

;-----------------------------------------------------------------------------------

;Decide if given input is a function.
(define Function?  
  (lambda (L)
    (cond ((null? L) #t)
          ((equal? (length L) 1) #t)
          ((member? (car (Builder2 L)) (cdr (Builder2 L))) #f)
          (else (Function? (cdr (Builder2 L)))))))

;-----------------------------------------------------------------------------------

;Decide if given input is surjective.
(define Surjective?
  (lambda (L S)
    (cond ((null? L) #f)
          ((null? S) #t)
          ((member? (car S) (Builder L)) (Surjective? L (cdr S)))
          (else #f))))

;-----------------------------------------------------------------------------------

;Decide if given input is injective.
(define Injective?
  (lambda (L S)
    (cond ((null? L) #t) 
          ((member? (car (Builder L)) (cdr (Builder L))) #f)
          (else (Injective? (cdr L) S)))))

;-----------------------------------------------------------------------------------  

;Decide if given input is reflexive.
(define Reflexive?
  (lambda (L S)
    (cond ((null? L) #t)
          ((equal? (car (car L)) (car (cdr (car L)))) (Reflexive? (cdr L) S))
          (else #f))))

;-----------------------------------------------------------------------------------

;Decide if given input is symmetric.
(define Symmetric?
  (lambda (L)
    (cond ((null? L) #t)
          ((equal? (car L) (reverse (car (cdr L)))) (Symmetric? (remove (car (cdr L)) (cdr L))))
          ((equal? (car L) (reverse (car (cdr (cdr L))))) (Symmetric? (remove (car (cdr (cdr L))) (cdr L))))
          (else #f))))

;Helper function that reverses a binary element.
(define reverse
  (lambda (L) (reverse-helper L '())))

;Helper function for reverse.
(define reverse-helper
  (lambda (L A)
    (if (null? L) A (reverse-helper (cdr L) (cons (car L) A)))))

;-----------------------------------------------------------------------------------

;Decide if given input is transitive.
(define Transitive?
  (lambda (L)
    (cond ((null? L) #t)
          ((member? (car (cdr (car L))) (Builder2 L)) (Transitive? (cdr L)))  
          (else #f))))

;-----------------------------------------------------------------------------------

;Compose two given binary lists.
(define Compose
  (lambda (L S)
    (cond ((null? S) L)
          ((member? (car S) L)
          (Compose L (cdr S)))
          (#t (Compose (cons (car S) L) (cdr S))))))