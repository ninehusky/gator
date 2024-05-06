#lang rosette

;;; Gator language definition.

(provide (struct-out var)
         (struct-out op))

(struct var (name-id bitwidth-id) #:transparent)
(struct op (op op-args children) #:transparent)
