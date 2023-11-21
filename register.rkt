#lang rosette

(require rosette/lib/synthax)

;;; (output-smt "test.smt2")

(define-symbolic init (bitvector 32))

(define (register d)
  (lambda (t) (if (= t 0) 0 (d (- t 1)))))

(define (lift op . args)
  (lambda (t) (apply op (map (lambda (stream) (stream t)) args))))

; args = [a, b]
; a = [a0, a1, a2, ...]
; b = [b0, b1, b2, ...]
; (map (lambda (stream) (stream t)) args) --> apply op [at, bt]

(define (addand0 a b c)
  (lift bvand (register (lift bvadd (register a) (register b))) (register (register c))))

(define (addand1 a b c)
  (register (register (lift bvand (lift bvadd a b) c))))

(define (addand2 a b c)
  (lift bvand (register (register (lift bvadd a b))) (register (register c))))

(define-symbolic chooser boolean?)
(define (addand3 a b c)
  (register (register (lift bvand (lift bvadd (if chooser a #f) b) c))))

(define-symbolic a b c (~> integer? (bitvector 32)))
(define-symbolic t integer?)

;;; (define fix-addand1
;;;   (synthesize #:forall (list a b c t)
;;;               #:guarantee (begin
;;;                             (assume (> t 1))
;;;                             (assert (bveq ((addand1 a b c) t) ((addand0 a b c) t))))))

(verify (begin
          (assume (> t 1))
          (assert (bveq ((addand0 a b c) t) ((addand3 a b c) t)))))
;;; (clear-vc!)
;;; (define cex (solve (assert (exists (list t) (bveq ((addand1 a b c) t) (bv 0 32))))))

;;; (evaluate a fix-addand1)

; idea 1: write LUT as rosette expression, use it in multiple places + use rosette to fill in LUT-based design (w/ regs)
;         (reg (and ..)) --> (reg (LUT <mem to fill in>))

; idea 2: mini-DSP; replicate diagram in section 2 of asplos paper (ALU is choose?)
;         bake idea of customizable # pipeline stages into DSP (with )
