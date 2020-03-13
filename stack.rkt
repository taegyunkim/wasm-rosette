#lang rosette

(require rosette/lib/synthax)
(require rosette/lib/angelic)
(require rosette/lib/match)

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
(current-bitwidth 32)

(struct i32.add () #:transparent)
(struct i32.shl () #:transparent)
(struct i32.const (c) #:transparent)
(struct local.get (i) #:transparent)

;; Manipulating list
(define l (list 1 2 3))
(define one (first l))
(define two (second l))
(define three (rest (rest l)))

(assert (equal? one 1))
(assert (equal? two 2))
(assert (equal? three '(3)))

;; Simple evaluator for binops
(define (do-binop instr lhs rhs)
  (match instr
    [(i32.add) (bvadd lhs rhs)]
    [(i32.shl) (bvshl lhs rhs)]
  )
)

(assert (equal? (do-binop (i32.add) (bv 3 32) (bv 4 32)) (bv 7 32)))
(assert (equal? (do-binop (i32.shl) (bv 1 32) (bv 2 32)) (bv 4 32)))

(assert
  (equal?
    (cons (do-binop (i32.add) (bv 3 32) (bv 4 32)) (list (bv 8 32)))
    (list (bv 7 32) (bv 8 32))
  )
)

;; Interpreter for above instructions
(define (interpret instrs locals)
  (define (interpret-instr instr stack)
    (match instr
      [(i32.add)
        (define result
          (bvadd (second stack) (first stack))
        )
        (cons result (drop stack 2))
      ]
      [(i32.shl)
        (define result
          (bvshl (second stack) (first stack))
        )
        (cons result (drop stack 2))
      ]
      [(i32.const c)
        (cons (integer->bitvector c (bitvector 32)) stack)
      ]
      [(local.get i)
        (cons (vector-ref locals i) stack)
      ]
    )
  )

  (foldl interpret-instr empty instrs)
)

;; Now define spec program
(define prog
  (list (local.get 0) (local.get 0) (i32.add))
)

;; Test the interpreter
(assert
  (equal?
    (interpret prog (list->vector (list (bv 2 32))))
    (list (bv 4 32))
  )
)

;; Spec program using shl
(define prog-shl
  (list (local.get 0) (i32.const 1) (i32.shl))
)

(assert
  (equal?
    (interpret prog-shl (list->vector (list (bv 2 32))))
    (list (bv 4 32))
  )
)

(define-symbolic c integer?)

;; A sketch to check whether hole based synthesis works with abaove interpreter.
(define sketch
  (list (local.get 0) (i32.const c) (i32.shl))
)

(define-symbolic x (bitvector 32))
(define locals (list->vector (list x)))

(define M
  (synthesize
    #:forall (list x)
    #:guarantee (assert
      (equal?
        (interpret prog locals)
        (interpret sketch locals)
      )
    )
  )
)

(evaluate sketch M)

;; Given n, generates a list of symbolic instructions
(define (synthesizer n)
  (for/list ([i n])
    (choose* (i32.shl) (i32.const (??)) (local.get (??)))
  )
)

(assert (equal? (length (synthesizer 3)) 3))

(define-symbolic y (bitvector 32))
(define ylocals (list->vector (list y)))

(define candidate (synthesizer (length prog)))

(define model
  (synthesize
    #:forall (list y)
    #:guarantee (assert
      (equal?
        (interpret prog ylocals)
        (interpret candidate ylocals)
      )
    )
  )
)

(evaluate candidate model)

(module+ test
  ;; Any code in this `test` submodule runs when this file is run using DrRacket
  ;; or with `raco test`. The code here does not run when this file is
  ;; required by another module.

  (check-equal? (+ 2 2) 4)
)
