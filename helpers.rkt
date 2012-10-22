#lang racket

(provide for/max define/memo)


(define-syntax (for/max stx)
  (syntax-case stx ()
   [(_ clauses . defs+exprs)
    (with-syntax ([original stx])
      #'(for/fold/derived original ([current-max -inf.0]) clauses
           (define maybe-new-max (let () . defs+exprs))
           (if (> maybe-new-max current-max)
               maybe-new-max
               current-max)))]))


(define-syntax-rule (define/memo (name id) body ...)
  (begin 
    (define table (make-hash))
    (define (name id)
      (cond
        [(hash-has-key? table id)
         (hash-ref table id)]
        [else
         (define answer (begin body ...))
         (hash-set! table id answer)
         answer]))))



(module+ test
  (require rackunit)
  (check-equal? (for/max ([i (in-list '())])
                i)
                -inf.0)
  (check-equal? (for/max ([i (in-list '(3 1 4 1 5 9 2 6))])
                         i)
                9)                    
  (check-equal? (for/max [(i (in-range 1 23))]
                         i)
                22)
  
  (check-equal? (for/max ([n (in-list '(3.14159 2.71828 1.61803))]
                          [s (in-list '(-1      1       1))])
                         (* n s))
                2.71828))
