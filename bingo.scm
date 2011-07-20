;; Utility functions

(define (contains? x l)
	(if (null? l)
		#f
		(if (= x (car l))
			#t
			(contains? x (cdr l)))))

(define (flatten list)
  (cond ((null? list) '())
		((list? (car list)) (append (flatten (car list)) (flatten (cdr list))))
			(else
				 (cons (car list) (flatten (cdr list))))))

(define (remove-elem l i)
	(remove-elem-rec l i 0 ))

(define (remove-elem-rec l i j)
	(if (>= j (length l))
		l
		(if (= i j)
			(cdr l)
			(cons (car l) (remove-elem-rec (cdr l) i (+ j 1))))))

(define (number->binary-list i b)
	(let loop ((c 0) (l '()))
		(if (= c b)
			l
			(loop (+ c 1) 
				(cons (if (zero? (bitwise-and (arithmetic-shift 1 c) i)) '0 '1) l)))))
				
(define (split ls)
  (letrec ([split-h (lambda (ls ls1 ls2)
                      (cond
                        [(or (null? ls) (null? (cdr ls)))
                         (cons (reverse ls2) ls1)]
                        [else (split-h (cddr ls)
                                (cdr ls1) (cons (car ls1) ls2))]))])
    (split-h ls ls '())))

(define (merge pred ls1 ls2)
  (cond
    [(null? ls1) ls2]
    [(null? ls2) ls1]
    [(pred (car ls1) (car ls2))
     (cons (car ls1) (merge pred (cdr ls1) ls2))]
    [else (cons (car ls2) (merge pred ls1 (cdr ls2)))]))

(define (merge-sort pred ls)
  (cond
    [(null? ls) ls]
    [(null? (cdr ls)) ls]
    [else (let ([splits (split ls)])
            (merge pred
              (merge-sort pred (car splits))
              (merge-sort pred (cdr splits))))]))

(define (sort ls pred)
  (merge-sort pred ls))
				
(define (next-number port)
	(let ((n (read-line port)))
		(if (eof-object? n)
			n	
			(string->number n))))

(define (make-number-file-stream file-name)
	(let ((port (open-file file-name)))
		(lambda ()
			(let ((n (next-number port)))
				(if (eof-object? n)
					(close-port port))
					n))))

(define (read-number-stream stream proc merge i)
	(let ((b (stream)))
		(if (eof-object? b)
			i
			(read-number-stream stream proc merge (merge (proc b) i)))))

;; BINGO functions

(define (random-bingo-integer start mx ignore)
	(let ((r (+ (random-integer mx) start)))
		(if (not (contains? r ignore))
			r	
			(random-bingo-integer start mx ignore))))

(define (make-bingo-row dim mx ignore)
		(reverse (make-bingo-row-rec dim mx ignore '() 0)))

(define (make-bingo-row-rec dim mx ignore row count)
	(if (= count dim)
		row
		(let* ((start (+ (* count mx) 1))
					(rand-value (random-bingo-integer start mx ignore)))
			(make-bingo-row-rec 
				dim
				mx
				(cons rand-value ignore) ;; probably not necessary
				(cons rand-value row)	
				(+ count 1)))))

(define (make-bingo-table dim mx)
	(reverse (make-bingo-table-rec dim mx '() 0)))

(define (make-bingo-table-rec dim mx rows count)
	(if (= count dim)
		rows
		(let* ((ignore (flatten rows))
					(new-rows (cons (make-bingo-row dim mx ignore) rows)))
			(make-bingo-table-rec dim mx new-rows (+ count 1)))))

(define (make-bingo-sheet dim mx num)
	(make-bingo-sheet-rec dim mx num 0 '()))

(define (make-bingo-sheet-rec dim mx num count sheet)
	(if (= num count)
		sheet
		(let ((table (make-bingo-table dim mx)))
			(make-bingo-sheet-rec dim mx num (+ count 1) (cons table sheet)))))

(define (list->binomial l)
	(list->binomial-rec l 0))

(define (list->binomial-rec l b)
	(if (null? l)
		b
		(list->binomial-rec (cdr l) (bitwise-ior (arithmetic-shift 1 (car l)) b))))

(define (extract-horizontal-sets dim table set)
	(append table set))

(define (extract-verticle-set dim table s i j)
	(if (= dim j)
		(sort s <)
		(let*	((row (car table))
					(new-s (cons (list-ref row i) s)))
			(extract-verticle-set dim (cdr table) new-s i (+ j 1)))))

(define (extract-verticle-sets dim table set)
 	(extract-verticle-sets-rec dim table set 0))

(define (extract-verticle-sets-rec dim table set i)
	(if (= dim i)
		set
		(let ((s (extract-verticle-set dim table '() i 0)))
			(cons s (extract-verticle-sets-rec dim table set (+ i 1))))))

(define (extract-corner-set dim table set)
 	(let* ((upper (car table))
				(lower (caddr table))
				(ul (car upper))
				(ur (list-ref upper (- dim 1)))
				(ll (car lower))
				(lr (list-ref lower (- dim 1))))
		(cons (sort (list ul ur ll lr) <) set)))

(define (extract-diagonal-set-rec dim table s i op e)
	(if (= e i)
	 	(sort s <)	
		(let* ((row (car table))
					(new-s (cons (list-ref row i) s)))
			(extract-diagonal-set-rec dim (cdr table) new-s (op i 1) op e))))

(define (extract-lr-set dim table set)
	(cons (extract-diagonal-set-rec dim table '() 0 + dim) set))

(define (extract-rl-set dim table set)
	(cons (extract-diagonal-set-rec dim table '() (- dim 1) - -1) set))

(define (extract-cross-set dim table set)
	(let ((lr (extract-lr-set dim table '()))
				(rl (extract-rl-set dim table '()))
				(i (- (/ (+ dim 1) 2) 1)))
		(cons (sort (remove-elem (flatten (append lr rl)) i) <) set)))

(define (extract-bingo-sets dim table)
	(extract-horizontal-sets dim table 
		(extract-verticle-sets dim table 
			(extract-corner-set dim table 
				(extract-lr-set dim table 
					(extract-rl-set dim table 
						(extract-cross-set dim table '())))))))

(define (bingo-set->binomial-set bs)
	(map (lambda (x) 
		(list->binomial 
			(map (lambda (y) (- y 1)) x))) bs))

(define (find-winners-set binomial-set call)
	(let ((fp 0)
				(i 1))
		(for-each
			(lambda (x)
				(if (= x (bitwise-and call x))
					(set! fp (bitwise-ior fp i)))
				(set! i (arithmetic-shift i 1)))
			binomial-set)
		fp))

(define (bingo-3x3-map-stream stream bingo-table proc merge i)
	(let* ((bingo-set (extract-bingo-sets 3 bingo-table))
				(binomial-set (bingo-set->binomial-set bingo-set)))
		(read-number-stream 
			stream
			(lambda (call) (proc binomial-set call))
		  merge	
			i)))

(define (bingo-3x3-sheet-map-stream stream bingo-sheet proc merge i)
	(let* ((bingo-sets (map (lambda (x) (extract-bingo-sets 3 x)) bingo-sheet))
				(binomial-sets (map (lambda (x) (bingo-set->binomial-set x)) bingo-sets)))
		(read-number-stream
			stream
			(lambda (b)
				(map (lambda (bs) (proc bs b)) binomial-sets))
			merge
			i)))

; 0	0 0 0 0 0 0 0 0 0 0	8488950	{}
; 1	0 0 0 0 0 0 0 0 0 1	427482	H1
; 2	0 0 0 0 0 0 0 0 1 0	437478	H2
; 4	0 0 0 0 0 0 0 1 0 0	427482	H3
; 8	0 0 0 0 0 0 1 0 0 0	427482	V1
; 16	0 0 0 0 0 1 0 0 0 0	437478	V2
; 32	0 0 0 0 1 0 0 0 0 0	427482	V3
; 64	0 0 0 1 0 0 0 0 0 0	31824	C
; 128	0 0 1 0 0 0 0 0 0 0	400350	LR
; 256	0 1 0 0 0 0 0 0 0 0	400350	RL
; 960	1 1 1 1 0 0 0 0 0 0	18564	X

(define standard-classifier '#(none H1 H2 H3 V1 V2 V3 C LR RL X multiple))
(define winning-classifier '#(none L L L L L L C L L X multiple))
(define winning-symbols '#(none L C X multiple))

(define (winning-symbol->number n)
	(cond
		((eq? n 'none) 0)
		((eq? n 'L) 1)
		((eq? n 'C) 2)
		((eq? n 'X) 3)
		((eq? n 'multiple) 4)))

(define (classify-bingo-set set classifier)
	(case set
		((  0) (vector-ref classifier 0))
		((  1) (vector-ref classifier 1))
		((  2) (vector-ref classifier 2))
		((  4) (vector-ref classifier 3))
		((  8) (vector-ref classifier 4))
		(( 16) (vector-ref classifier 5))
		(( 32) (vector-ref classifier 6))
		(( 64) (vector-ref classifier 7))
		((128) (vector-ref classifier 8))
		((256) (vector-ref classifier 9))
		((960) (vector-ref classifier 10))
		(else  (vector-ref classifier 11))))

(define (contains-symbol l s)
	(if (null? l)
		#f
		(if (eq? s (car l))
			#t
			(contains-symbol (cdr l) s))))

(define (count-winning-symbols l)
	(let ((symbols (make-vector 5 0)))
		(for-each
			(lambda (x)
				(let* ((i (winning-symbol->number x))
							(value (vector-ref symbols i)))
					(vector-set! symbols i (+ value 1))))
				l)
		symbols))

(define (find-non-zero-symbol s)
	(let ((l (vector-length s)))
		(let loop ((i 1))	; skip 'none
			(if (>= i l)
				'error
				(if (> (vector-ref s i) 0)
					i
					(loop (+ i 1)))))))

(define (classify-bingo-3x3-sheet-detailed cs)
	(let* ((sorted-cs 
					(sort cs 
						(lambda (x y)
							(< (winning-symbol->number x) (winning-symbol->number y))))))
		sorted-cs))

(define (get-symbol-count symbol-count symbol)
	(vector-ref symbol-count (winning-symbol->number symbol)))

(define (classify-bingo-3x3-sheet-simple cs)
	(let ((symbol-count (count-winning-symbols cs)))
		(cond
			((> (get-symbol-count symbol-count 'multiple) 0) 'multiple)
			((= (get-symbol-count symbol-count 'none) 3)
				(vector-ref winning-symbols (find-non-zero-symbol symbol-count)))
			((= (get-symbol-count symbol-count 'none) 4) 'none)
			(else 'mixed))))

(define (classify-bingo-3x3-sheet-winners cs)
	(let* ((symbol-count (count-winning-symbols cs))
				(none-symbol-count (get-symbol-count symbol-count 'none)))
		(cond
			((> (get-symbol-count symbol-count 'multiple) 0) 'multiple)
			((= none-symbol-count 4) 'none)
			((= none-symbol-count 3)
				(vector-ref winning-symbols (find-non-zero-symbol symbol-count)))
				LAMb: more here!

(define (bingo-3x3-histogram set t classifier)
	(let* ((cs (classify-bingo-set set classifier))
				(v (table-ref t cs 0)))
		(begin
			(table-set! t cs (+ v 1))
			t)))

(define (bingo-3x3-sheet-histogram set t)
	(let* ((cs (map (lambda (x) (classify-bingo-set x winning-classifier)) set))
				(sheet-class (classify-bingo-3x3-sheet cs))
				(v (table-ref t sheet-class 0)))
		(begin
			(table-set! t sheet-class (+ v 1))
			t)))

(define (bingo-3x3-histogram-stream bingo-table classifier)
	(let*	((s (make-number-file-stream "27_11.txt"))
				(t (make-table))
				(histogram (lambda (set t) (bingo-3x3-histogram set t classifier))))
		(bingo-3x3-map-stream s bingo-table find-winners-set histogram t)	
		t))

(define (bingo-3x3-sheet-histogram-stream bingo-table)
	(let*	((s (make-number-file-stream "27_11.txt"))
				(t (make-table))
				(histogram (lambda (set t) (bingo-3x3-sheet-histogram set t))))
		(bingo-3x3-sheet-map-stream s bingo-table find-winners-set histogram t)	
		t))

(define (histogram->list h)
	(map 
		(lambda (x) 
			;(cons (number->binary-list (car x) 10) (cdr x)))
			(cons (car x) (cdr x)))
		(sort
			(table->list h)
			(lambda (x y)
				(< (cdr x) (cdr y))))))
				;(if (= (cdr x) (cdr y))
				;	(< (car x) (car y))
				;	(< (cdr x) (cdr y)))))))

(define (group-histogram-by-winners h)
	(let ((w (make-table)))
		(table-for-each
			(lambda (k v)
				(if (> (bit-count k) 1)
					(table-set! w 0 (+ v (table-ref w 0 0)))
					(table-set! w k v)))
			h)
		w))

(define (print-winners-set binomial-set num)
	(if (not (eof-object? num))
		(println (number->binary-list (find-winners-set binomial-set num) 10))))

(random-source-randomize! default-random-source)
;(define bingo '( (1 2 3) (11 12 13) (21 22 23)))
;(define t (make-table))
;(define bingo (make-bingo-table 3 9))
;(println bingo)
;(bingo-3x3-map-stream (make-number-file-stream "27_11.txt") bingo find-winners-set bingo-3x3-histogram t)
;(map (lambda (x) (cons (number->binary-list (car x) 27) (cdr x))) (table->list t))

(pretty-print (table->list (bingo-3x3-sheet-histogram-stream (make-bingo-sheet 3 9 4))))
(pretty-print (table->list (bingo-3x3-sheet-histogram-stream (make-bingo-sheet 3 9 4))))
(pretty-print (table->list (bingo-3x3-sheet-histogram-stream (make-bingo-sheet 3 9 4))))
(pretty-print (table->list (bingo-3x3-sheet-histogram-stream (make-bingo-sheet 3 9 4))))

