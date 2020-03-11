#lang rosette

(module+ test
  (require rackunit))

;; Notice
;; To install (from within the package directory):
;;   $ raco pkg install
;; To install (once uploaded to pkgs.racket-lang.org):
;;   $ raco pkg install <<name>>
;; To uninstall:
;;   $ raco pkg remove <<name>>
;; To view documentation:
;;   $ raco docs <<name>>
;;
;; For your convenience, we have included LICENSE-MIT and LICENSE-APACHE files.
;; If you would prefer to use a different license, replace those files with the
;; desired license.
;;
;; Some users like to add a `private/` directory, place auxiliary files there,
;; and require them in `main.rkt`.
;;
;; See the current version of the racket style guide here:
;; http://docs.racket-lang.org/style/index.html

;; Code here

;; current-bitwidth is set to #f (false) to be consistent with Racket's infinite-precision semantics
;; Setting this to a specific value like 5, would let us consider constants within specific range.
(current-bitwidth 5)

(struct i32.add (left right) #:transparent)
(struct i32.shl (left right) #:transparent)
(struct i32.const (c) #:transparent)
(struct local.get (i) #:transparent)

(define (interpret p s)
  (match p
    [(i32.add a b) (bvadd (interpret a s) (interpret b s))]
    [(i32.shl a b) (bvshl (interpret a s) (interpret b s))]
    [(i32.const c) (integer->bitvector c (bitvector 32))]
    [(local.get i) (vector-ref s i)]))

;; Define a symbolic 32 bit bitvector
(define-symbolic x (bitvector 32))
(define-symbolic c integer?)

;; An example synthesizing x
(solve (assert 
  (bveq (bvshl (bv 4 (bitvector 32)) x)
    (bv 8 (bitvector 32)))
))

;; An example synthesizing c
(solve (assert
  (bveq (bvshl (bv 4 (bitvector 32)) (integer->bitvector c (bitvector 32)))
    (bv 8 (bitvector 32))
  ))
)

;; Define a sketch using a hole
(define sketch
  (i32.shl (local.get 0)  (i32.const c))
)

;; Define spec
(define spec
  (i32.add (local.get 0) (local.get 0))
)

;; Define list of locals using the symbolic bv variable
(define locals (list->vector (list x)))

;; Now try to synthesize c that satisfies all x, and the result is saved to the model.
(define M
  (synthesize
    #:forall (list x)
    #:guarantee (assert 
      (bveq
        (interpret spec locals)
        (interpret sketch locals)
      )
    )
  )
)

;; evaluate the sketch using model.
(evaluate sketch M)

(module+ test
  ;; Any code in this `test` submodule runs when this file is run using DrRacket
  ;; or with `raco test`. The code here does not run when this file is
  ;; required by another module.

  (check-equal? (+ 2 2) 4)
  (check-equal?
    (interpret
      (i32.add (i32.const 1) (i32.const 2))
      (list->vector '())
    )
    (bv 3 (bitvector 32))
  )

  (check-equal?
    (interpret
      (i32.shl (i32.const 5) (i32.const 2))
      (list->vector '())
    )
    (bv 20 (bitvector 32))
  )

  (check-equal?
    (interpret
      (i32.shl (i32.const 4) (local.get 0))
      (list->vector (list (bv 1 (bitvector 32))))
    )
    (bv 8 (bitvector 32))
  )
  

  (check-equal?
    (interpret
      (i32.shl (local.get 0) (local.get 0))
      (list->vector (list (bv 2 (bitvector 32))))
    )
    (bv 8 (bitvector 32))
  )
  
)
