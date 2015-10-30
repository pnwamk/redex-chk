#lang racket/base
(require racket/match
         racket/function
         racket/list
         redex/reduction-semantics
         rackunit
         syntax/parse/define
         (for-syntax racket/base
                     syntax/parse))


(begin-for-syntax
  (define-splicing-syntax-class strict-test
    #:commit
    #:attributes (unit fail-unit)
    [pattern (~seq #:f t:test)
             #:attr unit #'t.fail-unit
             #:attr fail-unit #'t.unit]
    [pattern (~seq #:t a:expr)
             #:attr unit
             (syntax/loc #'a
               (check-not-false (term a)))
             #:attr fail-unit
             (syntax/loc #'a
               (check-false (term a)))]
    [pattern (~seq #:= a:expr b:expr)
             #:attr unit
             (syntax/loc #'a
               (check-equal? (term a) (term b)))
             #:attr fail-unit
             (syntax/loc #'a
               (check-not-equal? (term a) (term b)))])

  (define-splicing-syntax-class test
    #:commit
    #:attributes (unit fail-unit)
    (pattern c:strict-test
             #:attr unit #'c.unit
             #:attr fail-unit #'c.fail-unit)
    (pattern (c:strict-test)
             #:attr unit #'c.unit
             #:attr fail-unit #'c.fail-unit)
    [pattern (~seq a:expr b:expr)
             #:with (c:strict-test) (syntax/loc #'a (#:= a b))
             #:attr unit #'c.unit
             #:attr fail-unit #'c.fail-unit]
    [pattern (~seq a:expr)
             #:with (c:strict-test) (syntax/loc #'a (#:t a))
             #:attr unit #'c.unit
             #:attr fail-unit #'c.fail-unit]))

(define-simple-macro (redex-chk e:test ...)
  (begin e.unit ...))

(provide redex-chk)

(module+ test
  (define-language Nats
    [Nat ::= Z (S Nat)])
  
  (define-metafunction Nats
    add2 : Nat -> Nat
    [(add2 Nat) (S (S Nat))])
  
  (define-judgment-form Nats
    #:mode (even I)
    #:contract (even Nat)
    [---------- "E-Zero"
                (even Z)]
    
    [(even Nat)
     ---------- "E-Step"
     (even (S (S Nat)))])
  
  (redex-chk
   Z Z
   #:f Z (S Z)
   #:t (even Z)
   #:f (even (S Z))
   #:f #:= (add2 Z) (S (S (S Z)))
   
   #:= (add2 (add2 (add2 Z)))
   (S (S (S (S (S (S Z))))))
   
   #:= (even (add2 (add2 (add2 Z))))
   (even (S (S (S (S (S (S Z)))))))
   
   #:f (even (S Z))))
