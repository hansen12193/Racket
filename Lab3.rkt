;Nathan Hansen
;CSCI301 Lab 3
#lang racket

;Returns the number of times that 'a' occurs in the function.
(define number-of-occurrences 
  (lambda (a B) 
    (number-of-occurrences-helper a B 0))) 
          
;Counter for number-of-occurences.
(define number-of-occurrences-helper 
  (lambda (a B n)
    (cond ((null? B) n) 
          ((eq? a (car B)) (number-of-occurrences-helper a (cdr B) (+ n 1))) 
          (else (number-of-occurrences-helper a (cdr B)  n)))))

;----------------------------------------------------

;Returns the truth value of “A if and only if B”.
(define iff
  (lambda (A B)
    (cond ((equal? A B) #t)
          (else #f))))

;----------------------------------------------------

;Returns the truth value of "A xor B".
(define exclusive-or
  (lambda (A B)
    (cond ((equal? A B) #f)
          (else #t))))

;----------------------------------------------------

;Returns the truth value of "A implies B".
(define implies
  (lambda (A B)
    (cond ((equal? A B) #t)
          ((and (equal? A #t) (equal? B #f)) #f)
          ((and (equal? A #f) (equal? B #t)) #t))))

;----------------------------------------------------
          
;Returns the list with the last element removed.
(define stem
  (lambda (L)
    (reverse (cdr (reverse L)))))
    
;Helper function for stem that reverses the list.
(define reverse
  (lambda (L) (reverse-helper L '())))

;Another helper function for reverse.
(define reverse-helper
  (lambda (L A)
    (if (null? L) A (reverse-helper (cdr L) (cons (car L) A)))))
  
;----------------------------------------------------

;Replicates all of the elements of a list.
(define replicate
  (lambda (L a)
    (cond ((null? L) L)
          ((equal? (car L) a) (cons a (cons a (replicate (cdr L) a))))
          (else (cons (car L) (replicate (cdr L) a))))))
