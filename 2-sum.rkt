#lang racket
(define (read-ints file-name size)
  (let* ([in (open-input-file file-name)]
	 [ht (make-hash)])
    (for ([n (in-range 0 (sub1 size))])
      (let ([num (read in)])
	(hash-set! ht num #t)))
    (printf "read-file: done\n")
    ht))

; find whether t has x + y representation
(define (has-sum? t ht)
  (for/first ([(x v) (in-hash ht)]
	      #:when (and (not (eq? (- t x) x))
			  (hash-has-key? ht (- t x)))
	      ) 
#t))

(define (run)
  (define cnt 0)

  (let ([ht (read-ints "/home/dc/code/algs/HashInt.txt" 500000)])
    (for ([t (in-range 2500 4001)])
      ;; (printf "~a " t)
      ;; (flush-output)
      (when  (has-sum? t ht)
	(set! cnt (add1 cnt)))))
  cnt)

(define (run2)
  (define cnt 0)
  (let ([ht (read-ints "tasdf.txt" 100)])
    (for ([t (in-range 30 (add1 60))])
      ;; (printf "~a\n " t)
      (flush-output)
      (when  (has-sum? t ht)
	(set! cnt (add1 cnt)))))
  cnt)

