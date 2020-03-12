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

(struct i32.add () #:transparent)
(struct i32.shl () #:transparent)
(struct i32.const (c) #:transparent)
(struct local.get (i) #:transparent)

(define l (list 1 2 3))
(define one (first l))
(define two (second l))
(define three (rest (rest l)))

(assert (equal? one 1))
(assert (equal? two 2))
(assert (equal? three '(3)))

(define (do-binop instr lhs rhs)
  (match instr
    [(i32.add) (bvadd lhs rhs)]
    [(i32.shl) (bvshl lhs rhs)]
  )
)

(assert (equal? (do-binop (i32.add) (bv 3 32) (bv 4 32)) (bv 7 32)))
(assert (equal? (do-binop (i32.shl) (bv 1 32) (bv 2 32)) (bv 4 32)))

(println (cons (do-binop (i32.add) (bv 3 32) (bv 4 32)) (list (bv 8 32))))

(define locals (list->vector (list (bv 2 32))))

(define (interpret instrs locals)
  (define (interpret-instr instr stack)
    (match instr
      [(i32.add) 
        (define result
          (bvadd (first stack) (second stack))
        )
        (cons result (drop stack 2))
      ]
      [(i32.shl) 
        (define result
          (bvshl (first stack) (second stack))
        )
        (cons result (drop stack 2))
      ]
      [(i32.const c)
        (cons (integer->bitvector c (bitvector 32)))
      ]
      [(local.get i)
        (cons (vector-ref locals i) stack)
      ]
    )
  )

  (foldl interpret-instr empty instrs)
)

(define prog
  (list (local.get 0) (local.get 0) (i32.add))
)

(assert 
  (equal? 
    (interpret prog locals) 
    (list (bv 4 32))
  )
)

(module+ test
  ;; Any code in this `test` submodule runs when this file is run using DrRacket
  ;; or with `raco test`. The code here does not run when this file is
  ;; required by another module.

  (check-equal? (+ 2 2) 4)  
)
