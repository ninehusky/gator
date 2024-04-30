#lang racket

(require json)

(provide json->gator)

;;; Returns a map from eclass->type, if there is one.
(define (type-inference egraph-json)
  (displayln (format "this is type inference"))
  (define nodes (dict-ref egraph-json 'nodes))
  (pretty-print nodes)
  ;;; Map from bv id to its bitwidth
  (define bv->bitwidth
    (for/hash ([(name node) (in-hash nodes)] #:when (equal? (dict-ref node 'op) "Bitvector"))
      (when (not (equal? (length (dict-ref node 'children)) 1))
        (error (format "there should be exactly 1 child in a bv node, there is ~a")))
      (let* ([children (dict-ref node 'children)]
             [bw-node (dict-ref nodes (string->symbol (first children)))]
             [bitwidth (string->number (dict-ref bw-node 'op))])
        (when (not (equal? (length (dict-ref bw-node 'children)) 0))
          (error
           (format
            "HasType of bv should be pointing to an int eclass, which has no children, but found ~a"
            (dict-ref bw-node 'children))))
        (values name bitwidth))))
  (define type-nodes
    (for/hash ([(name type-node) (in-hash nodes)] #:when (equal? (dict-ref type-node 'op) "HasType"))
      (match-let ([(list node-name type) (dict-ref type-node 'children)])
        (values node-name (dict-ref bv->bitwidth (string->symbol type))))))
  ;;; Map from id-node
  type-nodes)

(define (json->gator egraph-json)
  (type-inference egraph-json)
  (define pruned-class-data
    (for/hash ([(eclass value) (in-hash (dict-ref egraph-json 'class_data))]
               #:when (not (equal? eclass 'Unit-0)))
      (values eclass value)))
  (define pruned-nodes
    (for/hash ([(name node) (in-hash (dict-ref egraph-json 'nodes))]
               #:when (not (equal? (dict-ref node 'eclass) "Unit-0")))
      (values name node)))

  (when (not (jsexpr? egraph-json))
    (error 'json->gator "expected a JSON object, got ~s" egraph-json))

  ;;; eclass->id maps the keys in json['class_data'] to natural numbers
  (define eclass->gator-id
    (let ([eclass->id (make-hash)])
      (begin
        (for ([(key value) (in-hash pruned-class-data)])
          (hash-set! eclass->id key (hash-count eclass->id)))
        eclass->id)))

  (define gator-id->eclass
    (for/hash ([(eclass id) (in-hash eclass->gator-id)])
      (values id eclass)))

  ;;; node-name->id maps
  (define node-name->gator-id
    ;;; egraph-json['nodes'][name]['eclass']
    (for/hash ([(name node) (in-hash pruned-nodes)])
      (define eclass (dict-ref node 'eclass))
      (define gator-id (dict-ref eclass->gator-id (string->symbol eclass)))
      (values name gator-id)))

  (define (gen-gator-stmt eclass)
    ;;; find the nodes that have the given eclass
    (define eclass-nodes
      (for/list ([(_ node) (in-hash pruned-nodes)]
                 #:when (equal? (dict-ref node 'eclass) (symbol->string eclass)))
        node))
    (when (not (equal? (length eclass-nodes) 1))
      (error 'json->gator "expected exactly one node with eclass ~s, got ~s" eclass eclass-nodes))
    (first eclass-nodes))

  ;;; for i = 0 to len(eclass->gator-id) - 1
  ;;;   gen-gator-stmt(eclass)
  (for/list ([(eclass id) (in-hash eclass->gator-id)])
    (gen-gator-stmt eclass)))
