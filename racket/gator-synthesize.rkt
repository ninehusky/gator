#lang errortrace rosette

(require rackunit
         rosette
         rosette/lib/destruct
         (prefix-in gator: "gator-language.rkt")
         "gator-interpret.rkt")

(provide gator-synthesize)

(define spec-expr (list (gator:var 'a 4) (gator:var 'b 4) (gator:op bvadd (list) (list 0 1))))
(define sketch-expr (list (gator:var 'a 4) (gator:var 'INIT 4) (gator:op bvadd (list) (list 0 1))))

(define (gator-synthesize spec-expr sketch-expr inputs)
  (define env (make-hash))
  (hash-ref! env
             'a
             (lambda ()
               (define-symbolic* a (~> integer? (bitvector 4)))
               a))
  (hash-ref! env
             'b
             (lambda ()
               (define-symbolic* b (~> integer? (bitvector 4)))
               b))
  ;;; parameters are special: they remain constant.
  (hash-ref! env
             'INIT
             (lambda ()
               (define-symbolic* INIT (bitvector 4))
               (lambda (time) INIT)))

  (define (gator-id->stream env id prog-type)
    ;;; We'll keep this constant for now, but it'll be dynamic later.
    (define width 4)
    (define new-id
      (if (hash-has-key? env id)
          id
          (string->symbol (string-append prog-type " " (number->string id)))))
    (hash-ref! env
               new-id
               (lambda ()
                 (define-symbolic* x (~> integer? (bitvector width)))
                 x)))

  ;;; Comes up with an expression for the wire at time t.
  ;;; Also, returns an expression asserting that the expression is the same as the
  ;;; symbolic stream it represents in the environment.
  ;;; Returns (list expr assert-expr)
  (define (interpret prog id t prog-type)
    (define stmt (list-ref prog id))
    (define expr
      (destruct stmt
                [(gator:var id width) ((gator-id->stream env id prog-type) t)]
                [(gator:op op op-args op-children)
                 (match op
                   [else
                    (apply op
                           (map (lambda (child) ((gator-id->stream env child prog-type) t))
                                (append op-args op-children)))])]))

    (define assert-expr (bveq ((gator-id->stream env id prog-type) t) expr))

    (list expr assert-expr))

  (define-symbolic t integer?)

  (define (process-prog prog prog-type)
    (define exprs
      (for/list ([id (in-range (length prog))])
        (interpret prog id t prog-type)))
    (cons (map first exprs) (map second exprs)))

  (match-define (cons spec-interpreted-exprs spec-asserts) (process-prog spec-expr "spec"))

  (match-define (cons sketch-interpreted-exprs sketch-asserts) (process-prog sketch-expr "sketch"))

  (pretty-print spec-interpreted-exprs)
  (pretty-print sketch-interpreted-exprs)

  (pretty-print (bveq (last spec-interpreted-exprs) (last sketch-interpreted-exprs)))

  (synthesize #:forall (list t (hash-ref env 'a) (hash-ref env 'b))
              #:guarantee
              (begin
                (assume (>= t 0))
                (assume (bveq ((hash-ref env 'b) t) (bv 9 4)))
                (map (lambda (assert-expr) (assume assert-expr)) (append spec-asserts sketch-asserts))
                (assert (bveq (last spec-interpreted-exprs) (last sketch-interpreted-exprs))))))

(gator-synthesize spec-expr sketch-expr '())
