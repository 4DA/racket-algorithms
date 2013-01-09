#lang racket

(struct job (weight len))

(define (job> j1 j2)
  (define (diff j)
    ;; (/ (job-weight j) (job-len j)))
  ;; (define (diff j)
    (- (job-weight j) (job-len j)))
  (let ([diff-j1 (diff j1)]
	[diff-j2 (diff j2)])
    
    (cond [(> diff-j1 diff-j2) #t]
	  [(< diff-j1 diff-j2) #f]
	  [(= diff-j1 diff-j2) (> (job-weight j1) 
				  (job-weight j2))])))

(define (get-weighted-summ jobs)
  (define (get-sum-times)
    (for/fold ([sum 0] [time 0]) ([j jobs])
      (values (+ sum  (* (job-weight j) (+ time (job-len j))))
	      (+ time (job-len j)))))

  (let-values ([(sum times) (get-sum-times)])
    sum))

(define (schedule-greedy jobs)
  (let* ([sorted-jobs (sort jobs job>)])
    (get-weighted-summ sorted-jobs)))

(define (read-jobs jobs-num in)
  (for/list ([j jobs-num])
    (job (read in) (read in))))

(define (run) 
  (let* ([in (open-input-file "jobs.txt")]
	 [jobs-num (read in)]
	 [jobs (read-jobs jobs-num in)])
    (schedule-greedy jobs)))

; 69119377652 - minus
; 67311454237 - division

;;-----------------------------------------------------------------
