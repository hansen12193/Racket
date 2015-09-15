;Nathan Hansen
;CSCI301 Lab 6
#lang racket

;Function that will decide if statement is a tautology.
(define tautology?
  (lambda (L) 
    (cond ((member? (car (collect-prop-variables '(L))) L)
             (and (tautology? ((substitute '(L) '(car (collect-prop-variables '(L))) '#t) (substitute 'L '(car (collect-prop-variables '(L))) '#f)))))
          ((member? '(car (cdr (collect-prop-variables '(L)))) L)
             (and (tautology? ((substitute '(L) '(car (cdr (collect-prop-variables '(L)))) '#t) (substitute 'L '(car (cdr (collect-prop-variables '(L)))) '#f)))))
          ((equal? (evaluate-wff '(tautology? '(L))) #t) #t)
          (else #f))))

;Define a function that will extract only the prop variables and leave out the statements.
(define collect-prop-variables
  (lambda (L)
    (cond ((null? L) L)
          ((list? (car L)) (remove-duplicates (append (collect-prop-variables (car L)) (collect-prop-variables (cdr L)))))
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

;-------------------------------------------------------------------------------------------

;Evaluate a well-formed formula. 
(define evaluate-wff
  (lambda (W)
    ;If W is an atom, it is a truth value and should simply be returned.
    (cond ((atom? W) W)
          ;If W is of length one, it has extra parentheses; remove them with car.
          ((one? W) (evaluate-wff (car W)))
          ;If W is of length 2, it is of the form "not P" and one should return the negation of its evaluation.
          ((two? W) (not (evaluate-wff (cdr W))))
          ;If W is of length 3, it has a binary operator; evaluate the left and right hand sides and combine the results accordingly.
          ((three? W)
            (cond ((eq? (cadr W) 'and) (and (evaluate-wff (car W)) (evaluate-wff (caddr W))))
                  ((eq? (cadr W) 'or) (or (evaluate-wff (car W)) (evaluate-wff (caddr W))))
                  ((eq? (cadr W) 'implies) (or (not (evaluate-wff (car W))) (evaluate-wff (caddr W))))
                  ((eq? (cadr W) 'iff) (eq? (evaluate-wff (car W)) (evaluate-wff (caddr W)))))))))

;Define atom?
(define (atom? x) (not (or (pair? x) (null? x))))

;Define one?
(define one?
  (lambda (L)
    (cond ((equal? (length L) 1) #t)
          (else #f))))
    
;Define two?
(define two?
  (lambda (L)
    (cond ((equal? (length L) 2) #t)
          (else #f))))

;Define three?
(define three?
  (lambda (L)
    (cond ((equal? (length L) 3) #t)
          (else #f))))
