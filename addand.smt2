(set-logic ALL)

(declare-const init () (_ BitVec 32))

(define-fun register ((d (Array Int (_ BitVec 32)))) (Array Int (_ BitVec 32))
    ;;; Return concat([init], d)
    (concat (store d t init) d)
    ; (ite (= t 0) init (select d (- t 1)))
)

(define-fun register-at ((d (Array Int (_ BitVec 32)))) (Array Int (_ BitVec 32))
    (select d t init)
)

(define-fun helper-a ((t Int)) (_ BitVec 32) (register a t))
(define-fun helper-b ((t Int)) (_ BitVec 32) (register b t))

;;; t = 2
(define-fun addand0 ((t Int)) (_ BitVec 32)
    (bvand (bvadd (register helper-a t) (register (register b) t)))
)

d = (register a 1) --> (select a 0)
(register d 2) --> (select d 1)

(define-fun addand1 ((t Int)) (_ BitVec 32)

)



;;;; f(f(f(f(f(init, a[0]), a[1]), a[2]), a[3]), a[4])
