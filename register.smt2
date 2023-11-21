(set-logic ALL)

; Declare an infinite integer array representing data
; (declare-const data (Array Int (_ BitVec 32)))

; Now, make a register function that takes t. when t = 0, return initial value.
; when t = t' > 0, return data[t-1].
;
(define-fun init () (_ BitVec 32) (_ bv0 32))

(define-fun register0 ((d (Array Int (_ BitVec 32))) (t Int)) (_ BitVec 32)
    (ite (= t 0) init (select d (- t 1)))
)

(define-fun register1 ((d (Array Int (_ BitVec 32))) (t Int)) (_ BitVec 32)
    (ite (= t 0) init (select d (- t 1 0 0 0 0 0 0)))
)

; Now, assert that the (register t) = (register (t - 1)).
(assert
  (exists ((data (Array Int (_ BitVec 32))) (t Int))
    (and
        (>= t 0)
        ; (not (= (register data t) (select data (- t 1))))) 
        (not (= (register0 data t) (register1 data t))))
  )
)

; Check for satisfiability
(check-sat)

; Print the model if satisfiable
(get-model)

; 1. continue playing around with impls of reg0 and reg1 -- "how different can we get?"
; 2. play around with foralls
; 3. come up with example that 'proves' this is something we should do
; - idea: high level and low level implementation of a module
;   - ideally stateful, e.g., a 2-cycle muladd design