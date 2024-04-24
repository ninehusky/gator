#lang rosette

(require rosette/lib/synthax)
(require "utils.rkt")

;;; 2-cycle DSP
(define (DSP a b c d)
  (let* ([a (choose a (register a))]
         [b (choose b (register b) (register (register b)))]
         [c (choose c (register c) (register (register c)) (register (register (register c))))]
         [d (choose d (register d))]
         [pre-adder (lift (choose bvadd bvsub) a d)]
         [pre-adder (choose pre-adder (register pre-adder))]
         [multiplier (lift bvmul pre-adder b)]
         [multiplier (choose multiplier (register multiplier))]
         [op (lift (choose bvand bvadd bvsub) multiplier c)]
         [op (choose op (register op))])
    op))

;;; ((a + d) * b) & c
(define (addmuland-4cycle a b c d)
  (register (register (register (register (lift bvand (lift bvmul (lift bvadd a d) b) c))))))

(define (addmuland-3cycle a b c d)
  (register (register (register (lift bvand (lift bvmul (lift bvadd a d) b) c)))))

(define-symbolic a b c d (~> integer? (bitvector 32)))
(define-symbolic t integer?)

(synthesize #:forall (list a b c d t)
            #:guarantee (begin
                          (assume (> t 3))
                          (assert (bveq ((addmuland-3cycle b a c d) t) ((DSP a b c d) t)))))
