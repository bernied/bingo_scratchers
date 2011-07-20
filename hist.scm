(define (fact n) (if (<= n 1) 1 (* n (fact (- n 1)))))

(define (histogram-number n)
	(histogram-number-recursive n (make-vector 10)))

(define (histogram-number-recursive n h)
	(if (< n 1)
		h
		(let ((i (remainder n 10)))
			(vector-set! h i (+ (vector-ref h i) 1))
			(histogram-number-recursive (floor (/ n 10)) h))))
