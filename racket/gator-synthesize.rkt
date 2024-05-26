#lang errortrace rosette

(require rackunit
         rosette
         rosette/lib/destruct
         rosette/solver/smt/cvc5
         (prefix-in gator: "gator-language.rkt")
         "gator-interpret.rkt")

(provide gator-synthesize)

(current-solver (cvc5))

;;; (output-smt #t)

;;; (define spec-expr (list (gator:var 'mem-a 4) (gator:var 'clk 4)))
;;; (define sketch-expr (list (gator:var 'mem-b 4) (gator:var 'clk 4)))

(define spec-expr
  (list (gator:op bv (list) (list 8 4)) (gator:var 'clk 4) (gator:op 'reg (list 0 4) (list 1 0))))
(define sketch-expr
  (list (gator:var 'mem-b 4) (gator:var 'clk 4) (gator:op 'reg (list 0 4) (list 1 0))))

(define (gator-synthesize spec-expr sketch-expr inputs)
  (define env (make-hash))
  (hash-ref! env 'clk (lambda () (lambda (time) (if (odd? time) (bv 1 4) (bv 0 4)))))
  ;;;  (define-symbolic* clk (~> integer? (bitvector 4)))
  ;;;  clk))

  ;;; parameters are special: they remain constant.
  (hash-ref! env
             'mem-a
             (lambda ()
               (define-symbolic* mem-a (bitvector 4))
               (lambda (time) mem-a)))

  (hash-ref! env
             'mem-b
             (lambda ()
               (define-symbolic* mem-b (bitvector 4))
               (lambda (time) mem-b)))

  (define (gator-id->stream env id prog-type)
    ;;; We'll keep this width constant for now, but it'll be dynamic later.
    (define width 4)
    (define new-id
      (if (hash-has-key? env id)
          id
          (string->symbol (string-append prog-type "-" (number->string id)))))
    (hash-ref! env new-id (lambda () (constant new-id (~> integer? (bitvector width))))))

  ;;; Comes up with an expression for the wire at time t.
  ;;; Also, returns an expression asserting that the expression is the same as the
  ;;; symbolic stream it represents in the environment.
  ;;; Returns (list expr assume-expr)
  (define (interpret prog id t prog-type)
    (define stmt (list-ref prog id))
    (define assume-exprs (mutable-set))
    (define expr
      (destruct stmt
                [(gator:var var-id width) ((gator-id->stream env var-id prog-type) t)]
                [(gator:op op op-args op-children)
                 (match op
                   [(? (curry eq? bv)) (apply bv op-children)]
                   ['reg
                    (match-let* ([clk-id (first op-children)]
                                 ;;; The stream representing the clock.
                                 [clk-expr (gator-id->stream env clk-id prog-type)]
                                 [data-id (second op-children)]
                                 ;;; The stream representing the data.
                                 [data-expr (gator-id->stream env data-id prog-type)]
                                 [(list init-value width) op-args])
                      ;;; add an assumption that the expr outputs an initial value at time 0
                      ;;; (set-add! assume-exprs (bveq (data-expr 0) (bv init-value width)))
                      (if (= t 0) (bv init-value width) (data-expr (- t 1))))]
                   ;;; (if (= t 0)
                   ;;;     (bv init-value width)
                   ;;;     (if (and (bvzero? (clk-expr (- t 1))) (not (bvzero? (clk-expr t))))
                   ;;;         (data-expr (- t 1))
                   ;;;         ((gator-id->stream env id prog-type) (- t 1))))
                   ;;; comment
                   ;;;  )]
                   [else
                    (apply op
                           (map (lambda (child) ((gator-id->stream env child prog-type) t))
                                (append op-args op-children)))])]))

    (set-add! assume-exprs (bveq ((gator-id->stream env id prog-type) t) expr))

    ;;; (cons expr (set->list assume-exprs)))
    (list expr (bveq ((gator-id->stream env id prog-type) t) expr)))

  (define-symbolic t integer?)

  (define (process-prog prog prog-type)
    (define exprs
      (for/list ([id (in-range (length prog))])
        (interpret prog id t prog-type)))
    (cons (map first exprs) (map second exprs)))

  (match-define (cons spec-interpreted-exprs spec-asserts) (process-prog spec-expr "spec"))

  (match-define (cons sketch-interpreted-exprs sketch-asserts) (process-prog sketch-expr "sketch"))

  (displayln "spec-interpreted-exprs:")
  (pretty-print spec-interpreted-exprs)

  (displayln "sketch-interpreted-exprs:")
  (pretty-print sketch-interpreted-exprs)

  (displayln "spec-asserts:")
  (pretty-print spec-asserts)
  (pretty-print sketch-asserts)

  ;;; (pretty-print (bveq (car (interpret spec-expr 0 t "spec"))
  ;;;                     (car (interpret sketch-expr 0 t "sketch"))))

  (map (lambda (query) (assume query)) (flatten (append spec-asserts sketch-asserts)))

  (define model
    (synthesize #:forall (list t
                               (hash-ref env 'spec-0)
                               (hash-ref env 'spec-1)
                               (hash-ref env 'spec-2)
                               (hash-ref env 'sketch-0)
                               (hash-ref env 'sketch-1)
                               (hash-ref env 'sketch-2))
                #:guarantee
                (begin
                  (map (lambda (assert-expr) (assume assert-expr))
                       (flatten (append spec-asserts sketch-asserts)))
                  (assume (bveq (third spec-interpreted-exprs) (third sketch-interpreted-exprs)))
                  (assert (bveq (car (interpret spec-expr 2 (+ t 1) "spec"))
                                (car (interpret sketch-expr 2 (+ t 1) "sketch")))))))

  (assert (sat? model) "model is unsat")

  (displayln "results:")
  (displayln (last spec-interpreted-exprs))
  (displayln "sketch:")
  (displayln (last sketch-interpreted-exprs))

  (displayln (format "(spec 0): ~a" (evaluate (car (interpret spec-expr 2 0 "spec")) model)))
  (displayln (format "(spec 8): ~a" (evaluate (car (interpret spec-expr 2 8 "spec")) model)))
  (displayln (format "(sketch 0): ~a" (evaluate (car (interpret sketch-expr 2 0 "sketch")) model)))
  (displayln (format "(sketch 8): ~a" (evaluate (car (interpret sketch-expr 2 8 "sketch")) model)))

  model
  ;; comment
  )

(gator-synthesize spec-expr sketch-expr '())

;;; (bveq (ite (= 0 t)
;;;            (bv #x0 4)
;;;            (ite (&& (bveq (bv #x0 4) (app sketch-1 (+ -1 t))) (! (bveq (bv #x0 4) (app sketch-1 t))))
;;;                 (app sketch-0 (+ -1 t))
;;;                 (app sketch-2 (+ -1 t))))
;;;       (app sketch-2 t))
