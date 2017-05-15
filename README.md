A short-hand for writing redex tests, similar to the
[rackunit-chk](https://github.com/jeapostrophe/rackunit-chk) pkg.

The main feature besides providing short-hand for rackunit test
primitives is that all expressions ```e``` are automatically
wrapped by redex's ````term```, so they are redex expressions.

example:


```racket
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
   [#:f (S Z)]))
```
