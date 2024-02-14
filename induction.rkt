#lang rosette

(require rosette/lib/synthax)
(require "utils.rkt")

(define (interpret prog env id time cache)
  (if (< time 0) (error 'interpret "time must be non-negative") 0)
  (match (list-ref prog id)
    [`(Var ,x)
     (begin
       (when (not (symbol? x))
         (error 'interpret "expected a symbol"))
       (hash-ref env x))]
    [`(Op ,op ,ids ...)
     (apply op
            (for/list ([id ids])
              (interpret prog env id time cache)))]
    [`(Reg ,reg-id ,init)
     (displayln (format "time is ~a" time))
     (if (= time 0)
         (bv 1 1)
         (if (hash-has-key? cache (- time 1))
             (hash-ref cache (- time 1))
             (interpret prog env reg-id (- time 1) cache)))]
    [node
     (begin
       (when (not (bv? node))
         (error 'interpret "expected a bitvector"))
       node)]))

;;; use rosette to verify that the program always evaluates to 1
(define prog (list `(Reg ,0 ,(bv 1 1))))
(define-symbolic t integer?)

(define cache (make-hash))

(hash-set! cache (- t 1) (bv 1 1))

(define result (interpret prog '() 0 t cache))

(verify (begin
          ;;;   base case
          ;;;   (assume (bveq (interpret prog '() 0 0) (bv 1 1)))
          ;;;   inductive hypothesis
          ;;;   (assume (bveq (interpret prog '() 0 (+ -1 t)) (bv 1 1)))
          (assert (bveq (interpret prog '() 0 t cache) (bv 1 1)))))

(bveq (interpret prog '() 0 t cache) (bv 1 1))
