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

(struct I32.Add () #:transparent)
(struct I32.Sub () #:transparent)
(struct I32.Mul () #:transparent)
(struct I32.DivS () #:transparent)
(struct I32.DivU () #:transparent)
(struct I32.RemS () #:transparent)
(struct I32.RemU () #:transparent)
(struct I32.And () #:transparent)
(struct I32.Or () #:transparent)
(struct I32.Xor () #:transparent)
(struct I32.Shl () #:transparent)
(struct I32.ShrS () #:transparent)
(struct I32.ShrU () #:transparent)
(struct I32.Rotl () #:transparent)
(struct I32.Rotr () #:transparent)
(struct I32.Clz () #:transparent)
(struct I32.Ctz () #:transparent)
(struct I32.Popcnt () #:transparent)
(struct I32.Eqz () #:transparent)
(struct I32.Eq () #:transparent)
(struct I32.Ne () #:transparent)
(struct I32.LtS () #:transparent)
(struct I32.LtU () #:transparent)
(struct I32.LeS () #:transparent)
(struct I32.LeU () #:transparent)
(struct I32.GtS () #:transparent)
(struct I32.GtU () #:transparent)
(struct I32.GeS () #:transparent)
(struct I32.GeU () #:transparent)
(struct I32.Const (c) #:transparent)
(struct Local.Get (i) #:transparent)
(struct Local.Set (i) #:transparent)
(struct Local.Tee (i) #:transparent)

;; Interpreter for above instructions
(define (interpret instrs locals)
  (define (interpret-instr instr stack)
    (match instr
      [(I32.Add)
       (define result
         (bvadd (second stack) (first stack))
         )
       (cons result (drop stack 2))
       ]
      [(I32.Sub)
       (define result
         (bvsub (second stack) (first stack))
         )
       (cons result (drop stack 2))
       ]
      [(I32.Mul)
       (define result
         (bvmul (second stack) (first stack))
         )
       (cons result (drop stack 2))
       ]
      [(I32.DivS)
       (define result
         (bvsdiv (second stack) (first stack))
         )
       (cons result (drop stack 2))
       ]
      [(I32.DivU)
       (define result
         (bvudiv (second stack) (first stack))
         )
       (cons result (drop stack 2))
       ]
      [(I32.RemS)
       (define result
         (bvsrem (second stack) (first stack))
         )
       (cons result (drop stack 2))
       ]
      [(I32.RemU)
       (define result
         (bvurem (second stack) (first stack))
         )
       (cons result (drop stack 2))
       ]
      [(I32.And)
       (define result
         (bvand (second stack) (first stack))
         )
       (cons result (drop stack 2))
       ]
      [(I32.Or)
       (define result
         (bvor (second stack) (first stack))
         )
       (cons result (drop stack 2))
       ]
      [(I32.Xor)
       (define result
         (bvxor (second stack) (first stack))
         )
       (cons result (drop stack 2))
       ]
      [(I32.Shl)
       (define result
         (bvshl (second stack) (first stack))
         )
       (cons result (drop stack 2))
       ]
      [(I32.ShrS)
       (define result
         (bvashr (second stack) (first stack))
         )
       (cons result (drop stack 2))
       ]
      [(I32.ShrU)
       (define result
         (bvlshr (second stack) (first stack))
         )
       (cons result (drop stack 2))
       ]
      [(I32.Rotl)
       (define lhs (second stack))
       (define rhs (first stack))
       (define rotate_cnt (bvand rhs (bv 31 32)))
       (define result
         (bvor (bvshl lhs rotate_cnt) (bvlshr lhs (bvsub (bv 32 32) rotate_cnt))))
       (cons result (drop stack 2))]
      [(I32.Rotr)
       (define lhs (second stack))
       (define rhs (first stack))
       (define rotate_cnt (bvand rhs (bv 31 32)))
       (define result
         (bvor (bvlshr lhs rotate_cnt) (bvshl lhs (bvsub (bv 32 32) rotate_cnt))))
       (cons result (drop stack 2))]
      [(I32.Clz)
       (define (loop acc n)
         (if (bveq n (bv 0 32))
           32
           (if (bveq (bvand n (bvshl (bv 1 32) (bv 31 32))) (bv 0 32))
             (loop (+ acc 1) (bvshl n (bv 1 32)))
             acc)))
       (cons (bv (loop 0 (first stack)) 32) (drop stack 1))]
      [(I32.Ctz)
       (define (loop acc n)
         (if (bveq n (bv 0 32))
           32
           (if (bveq (bvand n (bv 1 32)) (bv 1 32))
             acc
             (loop (+ acc 1) (bvlshr n (bv 1 32))))))
       (cons (bv (loop 0 (first stack)) 32) (drop stack 1))]
      [(I32.Popcnt)
       (define (loop acc n)
         (if (bveq n (bv 0 32))
           acc
           (loop (if (bveq (bvand n (bv 1 32)) (bv 1 32)) (+ acc 1) acc) (bvlshr n (bv 1 32)))))
       (cons (bv (loop 0 (first stack)) 32) (drop stack 1))]
      [(I32.Eqz)
       (define result (if (bveq (stack first) (bv 0 32)) (bv 1 32) (bv 0 32)))
       (cons result (drop stack 1))]
      [(I32.Eq)
       (define lhs (second stack))
       (define rhs (first stack))
       (define result (if (bveq lhs rhs) (bv 1 32) (bv 0 32)))
       (cons result (drop stack 2))]
      [(I32.Ne)
       (define lhs (second stack))
       (define rhs (first stack))
       (define result (if (bveq lhs rhs) (bv 0 32) (bv 1 32)))
       (cons result (drop stack 2))]
      [(I32.LtS)
       (define lhs (second stack))
       (define rhs (first stack))
       (define result (if (bvslt lhs rhs) (bv 1 32) (bv 0 32)))
       (cons result (drop stack 2))]
      [(I32.LtU)
       (define lhs (second stack))
       (define rhs (first stack))
       (define result (if (bvult lhs rhs) (bv 1 32) (bv 0 32)))
       (cons result (drop stack 2))]
      [(I32.LeS)
       (define lhs (second stack))
       (define rhs (first stack))
       (define result (if (bvsle lhs rhs) (bv 1 32) (bv 0 32)))
       (cons result (drop stack 2))]
      [(I32.LeU)
       (define lhs (second stack))
       (define rhs (first stack))
       (define result (if (bvule lhs rhs) (bv 1 32) (bv 0 32)))
       (cons result (drop stack 2))]
      [(I32.GtS)
       (define lhs (second stack))
       (define rhs (first stack))
       (define result (if (bvsgt lhs rhs) (bv 1 32) (bv 0 32)))
       (cons result (drop stack 2))]
      [(I32.GtU)
       (define lhs (second stack))
       (define rhs (first stack))
       (define result (if (bvugt lhs rhs) (bv 1 32) (bv 0 32)))
       (cons result (drop stack 2))]
      [(I32.GeS)
       (define lhs (second stack))
       (define rhs (first stack))
       (define result (if (bvsge lhs rhs) (bv 1 32) (bv 0 32)))
       (cons result (drop stack 2))]
      [(I32.GeU)
       (define lhs (second stack))
       (define rhs (first stack))
       (define result (if (bvuge lhs rhs) (bv 1 32) (bv 0 32)))
       (cons result (drop stack 2))]
      [(I32.Const c)
       (cons (integer->bitvector c (bitvector 32)) stack)
       ]
      [(Local.Get i)
       (cons (vector-ref locals i) stack)
       ]
      [(Local.Set i)
       (define val (first stack))
       (vector-set! locals i val)
       (drop stack 1)]
      [(Local.Tee i)
       (define val (first stack))
       (vector-set! locals i val)
       stack]
      )
    )

  (foldl interpret-instr empty instrs)
  )

(define (syn spec)
  (define candidate
    (for/list ([i (length spec)])
      (apply choose*
             (shuffle
               (list (I32.Add) (I32.Sub) (I32.Mul) (I32.DivS) (I32.DivU) (I32.RemS) (I32.RemU) (I32.And) (I32.Or)
                     (I32.Xor) (I32.Shl) (I32.ShrS) (I32.ShrU) (I32.Const (??)) (Local.Get (??))))
             )
      )
    )

  (define-symbolic x (bitvector 32))
  (define-symbolic y (bitvector 32))
  (define locals (list->vector (list x y)))
  (define model
    (synthesize
      #:forall (list x y)
      #:guarantee (assert
                    (equal?
                      (interpret spec locals)
                      (interpret candidate locals)
                      )
                    )
      )
    )

  (evaluate candidate model)
  )

;; (syn (list (Local.Get 0) (Local.Get 1) (I32.Xor) (Local.Get 0) (Local.Get 1)
;;            (I32.And) (I32.And)))

(module+ test
  ;; Any code in this `test` submodule runs when this file is run using DrRacket
  ;; or with `raco test`. The code here does not run when this file is
  ;; required by another module.

  ;; Test the interpreter
  (check-equal?
    (interpret
      (list (Local.Get 0) (Local.Get 0) (I32.Add))
      (list->vector (list (bv 2 32)))
      )
    (list (bv 4 32))
    )

  (check-equal?
    (interpret
      (list (I32.Const 2) (I32.Const 1) (I32.Sub))
      (list->vector empty)
      )
    (list (bv 1 32))
    )

  (check-equal?
    (interpret
      (list (I32.Const 3) (I32.Const 5) (I32.Mul))
      (list->vector empty)
      )
    (list (bv 15 32))
    )

  (check-equal?
    (interpret
      (list (I32.Const -2) (I32.Const 1) (I32.DivS))
      (list->vector empty)
      )
    (list (bv -2 32))
    )

  (check-equal?
    (interpret
      (list (I32.Const -1) (I32.Const -1) (I32.DivU))
      (list->vector empty)
      )
    (list (bv 1 32))
    )

  (check-equal?
    (interpret
      (list (I32.Const -5) (I32.Const 2) (I32.DivU))
      (list->vector empty)
      )
    (list (bv #x7ffffffd 32))
    )

  (define x (bv 4 32))

  (check-equal?
    (interpret
      (list (Local.Get 0) (I32.Const 2) (I32.Mul))
      (list->vector (list x))
      )
    (list (bv 8 32))
    )

  (check-equal?
    x
    (bv 4 32)
    )

  (check-equal? (interpret (list (I32.Const 1) (I32.Const 1) (I32.Rotl))
                           (list->vector empty))
                (list (bv 2 32)))
  (check-equal? (interpret (list (I32.Const 1) (I32.Const 0) (I32.Rotl))
                           (list->vector empty))
                (list (bv 1 32)))
  (check-equal? (interpret (list (I32.Const -1) (I32.Const 1) (I32.Rotl))
                           (list->vector empty))
                (list (bv -1 32)))
  (check-equal? (interpret (list (I32.Const #xabcd9876) (I32.Const 1)
                                 (I32.Rotl))
                           (list->vector empty))
                (list (bv #x579b30ed 32)))


  ;; rotr tests
  (check-equal? (interpret (list (I32.Const 1) (I32.Const 0) (I32.Rotr))
                           (list->vector empty))
                (list (bv 1 32)))
  (check-equal? (interpret (list (I32.Const 1) (I32.Const 1) (I32.Rotr))
                           (list->vector empty))
                (list (bv #x80000000 32)))
  (check-equal? (interpret (list (I32.Const 1) (I32.Const 32) (I32.Rotr))
                           (list->vector empty))
                (list (bv 1 32)))

  (check-equal? (interpret (list (I32.Const 1) (I32.Const 1) (I32.Eq))
                           (list->vector empty))
                (list (bv 1 32)))

  ;; clz tests
  (check-equal? (interpret (list (I32.Const #xffffffff) (I32.Clz))
                           (list->vector empty))
                (list (bv 0 32)))
  (check-equal? (interpret (list (I32.Const 0) (I32.Clz))
                           (list->vector empty))
                (list (bv 32 32)))
  (check-equal? (interpret (list (I32.Const #x00008000) (I32.Clz))
                           (list->vector empty))
                (list (bv 16 32)))
  ;; ctz tests
  (check-equal? (interpret (list (I32.Const #xffffffff) (I32.Ctz))
                           (list->vector empty))
                (list (bv 0 32)))
  (check-equal? (interpret (list (I32.Const 0) (I32.Ctz))
                           (list->vector empty))
                (list (bv 32 32)))
  (check-equal? (interpret (list (I32.Const #x00008000) (I32.Ctz))
                           (list->vector empty))
                (list (bv 15 32)))
  ;; popcnt tests
  (check-equal? (interpret (list (I32.Const #xffffffff) (I32.Popcnt))
                           (list->vector empty))
                (list (bv 32 32)))
  (check-equal? (interpret (list (I32.Const 0) (I32.Popcnt))
                           (list->vector empty))
                (list (bv 0 32)))
  (check-equal? (interpret (list (I32.Const #x00008000) (I32.Popcnt))
                           (list->vector empty))
                (list (bv 1 32)))

  ;; local.set tests
  (check-equal? (interpret (list (I32.Const 5) (Local.Set 0) (Local.Get 0))
                           (list->vector (list (bv 0 32))))
                (list (bv 5 32)))
  (check-equal? (interpret (list (I32.Const 4) (Local.Tee 0) (Local.Get 0)
                                 (I32.Add))
                           (list->vector (list (bv 0 32))))
                (list (bv 8 32)))

  )
