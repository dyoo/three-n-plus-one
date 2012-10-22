#lang racket

(require "helpers.rkt")

(define/memo (cycle-length n)
  (cond
    [(= n 1)
     1]
    [(odd? n) 
     (add1 (cycle-length (add1 (* 3 n))))]
    [(even? n)
     (add1 (cycle-length (/ n 2)))]))


(define (max-cycle-length-range i j)
  (for/max ([n (in-range i (add1 j))])
    (cycle-length n)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ test
  (require rackunit)

  (check-equal? (cycle-length 1) 1)
  (check-equal? (cycle-length 2) 2)
  (check-equal? (cycle-length 4) 3)
  (check-equal? (cycle-length 5) 6)
  (check-equal? (cycle-length 22) 16)

  (check-equal? (max-cycle-length-range 1 10) 20)
  (check-equal? (max-cycle-length-range 100 200) 125)
  (check-equal? (max-cycle-length-range 201 210) 89)
  (check-equal? (max-cycle-length-range 900 1000) 174)
  (check-equal? (for/max [(i (in-range 900 (add1 1000)))]
                         (cycle-length i))
                174))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ main
  (for ([line (in-lines (current-input-port))])
    (define line-port (open-input-string line))
    (define i (read line-port))
    (define j (read line-port))
    (when (and (number? i)  (number? j))
      (printf "~a ~a ~a\n" i j (max-cycle-length-range i j)))))
