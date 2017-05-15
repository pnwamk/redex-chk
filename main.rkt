#lang racket/base
(require redex/reduction-semantics
         (rename-in rackunit [check-exn ru:check-exn] [check-not-exn ru:check-not-exn])
         racket/match
         syntax/parse/define
         (for-syntax racket/base
                     syntax/parse))


;; copy-pasted from chk
(define (exn-match x b)
  (match b
    [(? string? s)
     (exn-match x (regexp (regexp-quote s)))]
    [(? regexp? r)
     (regexp-match r (exn-message x))]
    [(? procedure? okay?)
     (okay? x)]))

(define-syntax-rule (check-exn a b)
  (ru:check-exn
   (λ (x) (exn-match x b))
   (λ () a)))

(define-syntax-rule (check-not-exn a b)
  (let ()
    (define av (->values a))
    (if (res:exn? av)
      (ru:check-false (exn-match (res:exn-x av) b))
      (ru:check-pred res:values? av))))

(begin-for-syntax
  (define-splicing-syntax-class (strict-test lang)
    #:commit
    #:attributes (unit fail-unit)
    [pattern (~seq #:f (~var t (test lang)))
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
               (check-not-equal? (term a) (term b)))]
    [pattern (~seq #:x a:expr exn-p:expr)
             #:attr unit
             (syntax/loc #'a
               (check-exn (term a) exn-p))
             #:attr fail-unit
             (syntax/loc #'a
               (check-not-exn (term a)))]
    ;; TODO: Would be nice to to use (default-language) but redex-match? requires an identifier
    [pattern (~seq #:m (~optional (~seq #:lang lang) #:defaults ([lang lang]))
                   a:expr b:expr)
             #:fail-unless (identifier? #'lang) "Cannot use #:m without providing a language."
             #:attr unit
             (syntax/loc #'a
               (check-true
                (redex-match? lang a (term b))))
             #:attr fail-unit
             (syntax/loc #'a
               (check-false
                (redex-match? lang a (term b))))]
    [pattern (~seq #:eq (~optional eq? #:defaults ([eq? #'(default-equiv)])) a:expr b:expr)
             #:attr unit
             (syntax/loc #'a
               (check eq? (term a) (term b)))
             #:attr fail-unit
             (syntax/loc #'a
               (check (compose not eq?) (term a) (term b)))]
    )

  (define-splicing-syntax-class (test lang)
    #:commit
    #:attributes (unit fail-unit)
    (pattern (~var c (strict-test lang))
             #:attr unit #'c.unit
             #:attr fail-unit #'c.fail-unit)
    (pattern ((~var c (strict-test lang)))
             #:attr unit #'c.unit
             #:attr fail-unit #'c.fail-unit)
    [pattern (~seq a:expr b:expr)
             #:with ((~var c (strict-test lang))) (syntax/loc #'a (#:= a b))
             #:attr unit #'c.unit
             #:attr fail-unit #'c.fail-unit]
    [pattern (~seq a:expr)
             #:with ((~var c (strict-test lang))) (syntax/loc #'a (#:t a))
             #:attr unit #'c.unit
             #:attr fail-unit #'c.fail-unit])

  (define-splicing-syntax-class (rel-test rel dargs)
    #:commit
    #:attributes (unit)
    [pattern (~and a [#:x args:expr ... exn-p])
             #:attr unit (quasisyntax/loc
                             #'a (check-exn (term (#,rel args ...)) exn-p))]
    [pattern (~and a [#:t args:expr ...])
             #:attr unit (quasisyntax/loc #'a (check-true (term (#,rel #,@dargs args ...))))]
    [pattern (~and a [#:f args:expr ...])
             #:attr unit (quasisyntax/loc #'a (check-false (term (#,rel #,@dargs args ...))))]
    [pattern (~and a [args:expr ...])
             #:attr unit (quasisyntax/loc #'a (check-true (term (#,rel #,@dargs args ...))))])

  (define-splicing-syntax-class (judg-holds-test rel dargs)
    #:commit
    #:attributes (unit)
    [pattern (~and a [#:x args:expr ... exn-p:expr])
             #:attr unit (quasisyntax/loc
                             #'a (check-exn (judgment-holds (#,rel args ...)) exn-p) )]
    [pattern (~and a [#:t args:expr ...])
             #:attr unit (quasisyntax/loc
                             #'a (check-true (judgment-holds (#,rel #,@dargs args ...))))]
    [pattern (~and a [#:f args:expr ...])
             #:attr unit (quasisyntax/loc
                             #'a (check-false (judgment-holds (#,rel #,@dargs args ...))))]
    [pattern (~and a [args:expr ...])
             #:attr unit (quasisyntax/loc
                             #'a (check-true (judgment-holds (#,rel #,@dargs args ...))))])

  (define-syntax-class rel-spec
    [pattern relation:id
             #:attr args '()]
    [pattern (relation:id . args:expr)]))

(define-syntax (redex-chk stx)
  (syntax-parse stx
    [(_ (~optional (~seq #:lang lang:id) #:defaults ([lang #'#f]))
        (~var e (test #'lang)) ...)
     #`(begin e.unit ...)]))

(define-syntax (redex-relation-chk stx)
  (syntax-parse stx
    [(_ relation:rel-spec
        (~var e (rel-test #'relation.relation (attribute relation.args))) ...)
     #`(begin e.unit ...)]))

(define-syntax (redex-judgment-holds-chk stx)
  (syntax-parse stx
    [(_ judgment:rel-spec
        (~var e (judg-holds-test #'judgment.relation (attribute judgment.args))) ...)
     #`(begin e.unit ...)]))



(provide redex-chk redex-relation-chk redex-judgment-holds-chk)

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

  (define-judgment-form Nats
    #:mode (equal-nats I I)
    #:contract (equal-nats Nat Nat)
    [---------- "Eq-Zero"
     (equal-nats Z Z)]

    [(equal-nats Nat_1 Nat_2)
     ---------- "Eq-Step"
     (equal-nats (S Nat_1) (S Nat_2))])

  (define-judgment-form Nats
    #:mode (pred I O)
    #:contract (pred Nat Nat)
    [---------- "Pred"
     (pred (S Nat) Nat)])

  (redex-chk #:lang Nats
   Z Z
   #:f Z (S Z)
   #:t (even Z)
   #:f (even (S Z))
   #:f #:= (add2 Z) (S (S (S Z)))

   #:= (add2 (add2 (add2 Z)))
   (S (S (S (S (S (S Z))))))
   #:x (add2 5) #rx"not in my domain"

   #:= (even (add2 (add2 (add2 Z))))
   (even (S (S (S (S (S (S Z)))))))

   #:f (even (S Z))

   #:m Nat Z
   #:m #:lang Nats Nat Z
   #:m Nat (S Z)
   #:f #:m Nat S

   #:eq Z Z
   #:f #:eq Z S
   #:eq eq? Z Z)

  (redex-relation-chk
   even
   [#:t Z]
   [#:f (S Z)]
   [(S (S Z))]
   [#:x 5 "input values do not match its contract"])

  (redex-relation-chk
   equal-nats
   [#:t Z Z]
   [#:f (S Z) Z]
   [(S (S Z)) (add2 Z)])

  (redex-relation-chk
   (equal-nats Z)
   [#:t Z]
   [#:f (S Z)]
   [#:f (add2 Z)])

  (redex-judgment-holds-chk
   pred
   [(S Z) Z]
   [(S (S Z)) (S Z)]
   [#:f Z any]
   [#:x 5 any exn:fail:redex?])

  (redex-judgment-holds-chk
   (pred (S Z))
   [Z]
   [#:f (S Z)]
   [#:f Z any]))
