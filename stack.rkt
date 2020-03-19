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

;; TODO(taegyunkim): Split out into syntax/ast.rkt
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
    ;; Interpret a binary operator.
    (define (interpret-binop op)
      (match-let-values ([((list rhs lhs) stack) (split-at stack 2)])
                        (cons (op lhs rhs) stack)))

    (define (interpret-unop op)
      (match-let-values ([((list operand) stack) (split-at stack 1)])
                        (cons (op operand) stack)))

    (define (interpret-relop op)
      (interpret-binop (lambda (lhs rhs) (if (op lhs rhs)
                                             (bv 1 32)
                                             (bv 0 32)))))

    (define (bvrotl lhs rhs)
      (define rotate_cnt (bvand rhs (bv 31 32)))
      (bvor (bvshl lhs rotate_cnt) (bvlshr lhs (bvsub (bv 32 32) rotate_cnt))))

    (define (bvrotr lhs rhs)
      (define rotate_cnt (bvand rhs (bv 31 32)))
      (bvor (bvlshr lhs rotate_cnt) (bvshl lhs (bvsub (bv 32 32) rotate_cnt))))

    ;; s: integer?
    ;; x: (bitvector s)
    ;; returns (bitvector s)
    (define (bvclz s x)
      (define (loop acc n)
        (if (bveq n (bv 0 s))
            s
            (if (bveq (bvand n (bvshl (bv 1 s) (bv (- s 1) s))) (bv 0 s))
                (loop (+ acc 1) (bvshl n (bv 1 s)))
                acc)))
      (bv (loop 0 x) s))

    (define (bvctz s x)
      (define (loop acc n)
        (if (bveq n (bv 0 s))
            s
            (if (bveq (bvand n (bv 1 s)) (bv 1 s))
                acc
                (loop (+ acc 1) (bvlshr n (bv 1 s))))))
      (bv (loop 0 x) s))

    (define (bvpopcnt s x)
      (define (loop acc n)
        (if (bveq n (bv 0 s))
            acc
            (loop (if (bveq (bvand n (bv 1 s)) (bv 1 s))
                      (+ acc 1) acc)
                  (bvlshr n (bv 1 s)))))
      (bv (loop 0 x ) s))

    (match instr
      [(I32.Add) (interpret-binop bvadd)]
      [(I32.Sub) (interpret-binop bvsub)]
      [(I32.Mul) (interpret-binop bvmul)]
      [(I32.DivS) (interpret-binop bvsdiv)]
      [(I32.DivU) (interpret-binop bvudiv)]
      [(I32.RemS) (interpret-binop bvsrem)]
      [(I32.RemU) (interpret-binop bvurem)]
      [(I32.And) (interpret-binop bvand)]
      [(I32.Or) (interpret-binop bvor)]
      [(I32.Xor) (interpret-binop bvxor)]
      [(I32.Shl) (interpret-binop bvshl)]
      [(I32.ShrS) (interpret-binop bvashr)]
      [(I32.ShrU) (interpret-binop bvlshr)]
      [(I32.Rotl) (interpret-binop bvrotl)]
      [(I32.Rotr) (interpret-binop bvrotr)]
      [(I32.Clz) (interpret-unop ((curry bvclz) 32))]
      [(I32.Ctz) (interpret-unop ((curry bvctz) 32))]
      [(I32.Popcnt) (interpret-unop ((curry bvpopcnt) 32))]
      [(I32.Eqz)
       (match-let-values ([((list operand) stack) (split-at stack 1)])
                         (define result (if (bveq operand (bv 0 32))
                                            (bv 1 32)
                                            (bv 0 32)))
                         (cons result stack))]
      [(I32.Eq) (interpret-relop bveq)]
      [(I32.Ne) (interpret-relop (not (bveq)))]
      [(I32.LtS) (interpret-relop bvslt)]
      [(I32.LtU) (interpret-relop bvult)]
      [(I32.LeS) (interpret-relop bvsle)]
      [(I32.LeU) (interpret-relop bvule)]
      [(I32.GtS) (interpret-relop bvsgt)]
      [(I32.GtU) (interpret-relop bvugt)]
      [(I32.GeS) (interpret-relop bvsge)]
      [(I32.GeU) (interpret-relop bvuge)]
      [(I32.Const c)
       (cons (integer->bitvector c (bitvector 32)) stack)]
      [(Local.Get i)
       (cons (vector-ref locals i) stack)]
      [(Local.Set i)
       (match-let-values ([((list operand) stack) (split-at stack 1)])
                         (vector-set! locals i operand)
                         stack)]
      [(Local.Tee i)
       (vector-set! locals i (first stack))
       stack]))

  (foldl interpret-instr empty instrs))

(define (syn spec)
  (define candidate
    (for/list ([i (length spec)])
      (apply choose*
             (shuffle
              (list (I32.Add) (I32.Sub) (I32.Mul) (I32.DivS) (I32.DivU)
                    (I32.RemS) (I32.RemU) (I32.And) (I32.Or) (I32.Xor)
                    (I32.Shl) (I32.ShrS) (I32.ShrU) (I32.Const (??))
                    (Local.Get (??)))))))

  (define-symbolic x (bitvector 32))
  (define locals (list->vector (list x)))
  (define model
    (synthesize
     #:forall (list x)
     #:guarantee (assert (equal? (interpret spec locals)
                                 (interpret candidate locals)))))

  (evaluate candidate model))

(module+ test
  ;; Any code in this `test` submodule runs when this file is run using
  ;; DrRacket or with `raco test`. The code here does not run when this file is
  ;; required by another module.

  ;; Test the interpreter
  (check-equal? (interpret (list (Local.Get 0) (Local.Get 0) (I32.Add))
                           (list->vector (list (bv 2 32))))
                (list (bv 4 32)))

  (check-equal? (interpret (list (I32.Const 2) (I32.Const 1) (I32.Sub))
                           (list->vector empty))
                (list (bv 1 32)))

  (check-equal? (interpret (list (I32.Const 3) (I32.Const 5) (I32.Mul))
                           (list->vector empty))
                (list (bv 15 32)))

  (check-equal? (interpret (list (I32.Const -2) (I32.Const 1) (I32.DivS))
                           (list->vector empty))
                (list (bv -2 32)))

  (check-equal? (interpret (list (I32.Const -1) (I32.Const -1) (I32.DivU))
                           (list->vector empty))
                (list (bv 1 32)))

  (check-equal? (interpret (list (I32.Const -5) (I32.Const 2) (I32.DivU))
                           (list->vector empty))
                (list (bv #x7ffffffd 32)))

  (define x (bv 4 32))

  (check-equal? (interpret (list (Local.Get 0) (I32.Const 2) (I32.Mul))
                           (list->vector (list x)))
                (list (bv 8 32)))

  (check-equal? x (bv 4 32))

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
                (list (bv 8 32))))

