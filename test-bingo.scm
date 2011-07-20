;;; This file implements the test cases for bingo.

;(load "testeez")
;(load "bingo")


(testeez
  "Bingo Tests"
	  
	;; (contains? x l)
	(test/equal "contains?"
		(contains? 1 '(3 2 1)) #t)

	(test/equal "contains?"
		(contains? 1 '(3 2 0)) #f)

	(test/equal "contains?"
		(contains? 1 '()) #f)

	;; (flatten list)
	(test/equal "flatten"
		(flatten '()) '())

	(test/equal "flatten"
		(flatten '(1 2 3)) '(1 2 3))

	(test/equal "flatten"
		(flatten '(1 (2 3) (4))) '(1 2 3 4))

	(test/equal "flatten"
		(flatten '((1) (2) (3))) '(1 2 3))

	(test/equal "flatten"
		(flatten '((((((1 (2) ((3))))))))) '(1 2 3))

	;; (remove-elem l i)
	(test/equal "remove-elem"
		(remove-elem '() 0) '())

	(test/equal "remove-elem"
		(remove-elem '() 100) '())

	(test/equal "remove-elem"
		(remove-elem '(1 2 3) 0) '(2 3))

	(test/equal "remove-elem"
		(remove-elem '(1 2 3) 1) '(1 3))

	(test/equal "remove-elem"
		(remove-elem '(1 2 3) 2) '(1 2))

	(test/equal "remove-elem"
		(remove-elem '(1 2 3) 3) '(1 2 3))

	(test/equal "remove-elem"
		(remove-elem '(1 2 3) 4) '(1 2 3))

	(test/equal "remove-elem"
		(remove-elem '(1) 0) '())

	(test/equal "remove-elem"
		(remove-elem '(1) 1) '(1))

	(test/equal "remove-elem"
		(remove-elem '(1 '(2 3) 4) 1) '(1 4))

	;; (number->binary-list i b)
	(test/equal "number->binary-list"
		(number->binary-list 0 3) '(0 0 0))
		
	(test/equal "number->binary-list"
		(number->binary-list 1 3) '(0 0 1))
		
	(test/equal "number->binary-list"
		(number->binary-list 2 3) '(0 1 0))
		
	(test/equal "number->binary-list"
		(number->binary-list 3 3) '(0 1 1))
		
	(test/equal "number->binary-list"
		(number->binary-list 4 3) '(1 0 0))
		
	(test/equal "number->binary-list"
		(number->binary-list 5 3) '(1 0 1))
		
	(test/equal "number->binary-list"
		(number->binary-list 6 3) '(1 1 0))
		
	(test/equal "number->binary-list"
		(number->binary-list 7 3) '(1 1 1))
		
	(test/equal "number->binary-list"
		(number->binary-list 8 3) '(0 0 0))
		
	(test/equal "number->binary-list"
		(number->binary-list 8 4) '(1 0 0 0))
		
	(test/equal "number->binary-list"
		(number->binary-list 1 0) '())
		
	(test/equal "number->binary-list"
		(number->binary-list 0 1) '(0))
		
	(test/equal "number->binary-list"
		(number->binary-list 1 1) '(1))
		
	(test/equal "number->binary-list"
		(number->binary-list 0 32) '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
		
	(test/equal "number->binary-list"
		(number->binary-list 2147483647 32) '(0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1))
		
	(test/equal "number->binary-list"
		(number->binary-list 4294967295  32) '(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1))
	
	;; split
	;; merge
	;; merge-sort
	;; (sort ls pred)
	(test/equal "sort"
		(sort '(3 2 5 1 8 0) <) '(0 1 2 3 5 8))
	
	(test/equal "sort"
		(sort '(3 2 5 1 8 0) >) '(8 5 3 2 1 0))
	
	(test/equal "sort"
		(sort '(1 2 3) <) '(1 2 3))
	
	(test/equal "sort"
		(sort '(1 2 3) >) '(3 2 1))
	
	(test/equal "sort"
		(sort '(1 1 2 3) <) '(1 1 2 3))
	
	(test/equal "sort"
		(sort '(1 1 2 3) >) '(3 2 1 1))
	
	(test/equal "sort"
		(sort '() <) '())
	
	(test/equal "sort"
		(sort '(1) <) '(1))
	
	;; (next-number port)
	;; (make-number-file-stream file-name)
	(test-define "file stream"
		fs (make-number-file-stream "test_123.txt"))

	(test/equal "make-number-file-stream"
		(fs) 1)

	(test/equal "make-number-file-stream"
		(fs) 2)

	(test/equal "make-number-file-stream"
		(fs) 3)

	(test/equal "make-number-file-stream"
		(fs) #!eof)

	;; (make-number-list-stream l)
	(test-define "number stream"
		ls (make-number-list-stream '(1 2 3)))

	(test/equal "make-number-file-stream"
		(ls) 1)

	(test/equal "make-number-file-stream"
		(ls) 2)

	(test/equal "make-number-file-stream"
		(ls) 3)

	(test/equal "make-number-file-stream"
		(ls) #!eof)

	(test/equal "make-number-file-stream"
		(ls) #!eof)

	;; (read-number-stream stream proc merge i)
	
	;; (random-bingo-integer start mx ignore)
	(test/equal "random-bingo-integer"
		(random-bingo-integer 0 1 '()) 0)

	(test/equal "random-bingo-integer"
		(random-bingo-integer 5 1 '()) 5)
		
	(test-define "random number"
		rn (random-bingo-integer 0 10 '()))

	(test/equal "random-bingo-integer"
		(and (>= rn 0) (< rn 10)) #t)
		
	(test/equal "random-bingo-integer"
		(random-bingo-integer 0 3 '(1 2)) 0)
		
	;; (make-bingo-row dim mx ignore)
	(test-define "make-bingo-row "
		br (make-bingo-row 3 9 '(3 12 21)))
		
	(test/equal "make-bingo-row"
		(length br) 3)
		
	(test/equal "make-bingo-row"
		(and (>= (car br) 1) (<= (car br) 9) (not (= (car br) 3))) #t)

	(test/equal "make-bingo-row"
		(and (>= (cadr br) 10) (<= (cadr br) 18) (not (= (car br) 12))) #t)

	(test/equal "make-bingo-row"
		(and (>= (caddr br) 19) (<= (caddr br) 27) (not (= (car br) 21))) #t)

	(test/equal "make-bingo-row"
		(length (make-bingo-row 5 15 '())) 5)
	
	;; (make-bingo-table dim mx)
	(test-define "make-bingo-table"
		bt3 (make-bingo-table 3 9))
		
	(test/equal "make-bingo-table"
		(length bt3) 3)

	(test/equal "make-bingo-table"
		(length (flatten bt3)) 9)

	(test/equal "make-bingo-table"
		(length (car bt3)) 3)

	(test/equal "make-bingo-table"
		(length (cadr bt3)) 3)

	(test/equal "make-bingo-table"
		(length (caddr bt3)) 3)

	(test-define "make-bingo-table"
		bt5 (make-bingo-table 5 15))
		
	(test/equal "make-bingo-table"
		(length bt5) 5)

	(test/equal "make-bingo-table"
		(length (flatten bt5)) 25)

	(test/equal "make-bingo-table"
		(length (car bt5)) 5)

	(test/equal "make-bingo-table"
		(length (cadr bt5)) 5)

	(test/equal "make-bingo-table"
		(length (caddr bt5)) 5)

	;; (make-bingo-sheet dim mx num)
	(test-define "make-bingo-sheet "
		bs3x4 (make-bingo-sheet 3 9 4))

	(test/equal "make-bingo-sheet"
		(length bs3x4) 4)

	(test/equal "make-bingo-sheet"
		(length (flatten bs3x4)) (* 3 3 4))

	(test/equal "make-bingo-sheet"
		(length (car bs3x4)) 3)

	(test/equal "make-bingo-sheet"
		(length (caar bs3x4)) 3)

	(test/equal "make-bingo-sheet"
		(length (cadr bs3x4)) 3)

	(test/equal "make-bingo-sheet"
		(length (caddr bs3x4)) 3)
		
	;; can add more tests for bingo-sheet!!!
	
	;; (list->binomial l)
	(test/equal "list->binomial"
		(list->binomial '()) 0)

	(test/equal "list->binomial"
		(list->binomial '(0)) 1)

	(test/equal "list->binomial"
		(list->binomial '(1)) 2)

	(test/equal "list->binomial"
		(list->binomial '(2)) 4)

	(test/equal "list->binomial"
		(list->binomial '(4)) 16)

	(test/equal "list->binomial"
		(list->binomial '(32)) 4294967296)

	(test/equal "list->binomial"
		(list->binomial '(0 1 2 3)) 15)

	(test/equal "list->binomial"
		(list->binomial '(0 31)) 2147483649)

	;; (extract-horizontal-sets dim table set)
	(test-define "3x3 table"
		tbl3 '((1 2 3) (4 5 6) (7 8 9)))
	
	(test-define "5x5 table"
		tbl5 '((1 2 3 4 5) (6 7 8 9 10) (11 12 13 14 15) (16 17 18 19 20) (21 22 23 24 25)))
	
	(test/equal "extract-horizontal-sets"
		(extract-horizontal-sets 3 tbl3 '()) tbl3)
		
	(test/equal "extract-horizontal-sets"
		(extract-horizontal-sets 3 tbl3 '(foo)) (append tbl3 '(foo)))
		
	(test/equal "extract-horizontal-sets"
		(extract-horizontal-sets 5 tbl5 '()) tbl5)
		
	;; (extract-verticle-sets dim table set)
	(test/equal "extract-verticle-sets"
		(extract-verticle-sets 3 tbl3 '()) '((1 4 7) (2 5 8) (3 6 9)))
	
	(test/equal "extract-verticle-sets"
		(extract-verticle-sets 3 tbl3 '(foo)) '((1 4 7) (2 5 8) (3 6 9) foo))
	
	(test/equal "extract-verticle-sets"
		(extract-verticle-sets 5 tbl5 '()) '((1 6 11 16 21) (2 7 12 17 22) (3 8 13 18 23) (4 9 14 19 24) (5 10 15 20 25)))
	
	;; (extract-corner-set dim table set)
	(test/equal "extract-corner-set"
		(extract-corner-set 3 tbl3 '()) '((1 3 7 9)))

	(test/equal "extract-corner-set"
		(extract-corner-set 3 tbl3 '(foo)) '((1 3 7 9) foo))
		
	(test/equal "extract-corner-set"
		(extract-corner-set 5 tbl5 '()) '((1 5 21 25)))
	
	;; (extract-lr-set dim table set)
	(test/equal "extract-lr-set"
		(extract-lr-set 3 tbl3 '()) '((1 5 9)))

	(test/equal "extract-lr-set"
		(extract-lr-set 3 tbl3 '(foo)) '((1 5 9) foo))

	(test/equal "extract-lr-set"
		(extract-lr-set 5 tbl5 '()) '((1 7 13 19 25)))

	;; (extract-rl-set dim table set)
	(test/equal "extract-rl-set"
		(extract-rl-set 3 tbl3 '()) '((3 5 7)))
		
	(test/equal "extract-rl-set"
		(extract-rl-set 3 tbl3 '(foo)) '((3 5 7) foo))
		
	(test/equal "extract-rl-set"
		(extract-rl-set 5 tbl5 '()) '((5 9 13 17 21)))
	
	;; (extract-cross-set dim table set)
	(test/equal "extract-cross-set"
		(extract-cross-set 3 tbl3 '()) '((1 3 5 7 9)))
	
	(test/equal "extract-cross-set"
		(extract-cross-set 3 tbl3 '(foo)) '((1 3 5 7 9) foo))

	(test/equal "extract-cross-set"
		(extract-cross-set 5 tbl5 '()) '((1 5 7 9 13 17 19 21 25)))

	;; (extract-bingo-sets dim table)
	(test/equal "extract-bingo-sets"
		(extract-bingo-sets 3 tbl3) '((1 2 3) (4 5 6) (7 8 9) (1 4 7) (2 5 8) (3 6 9) (1 3 7 9) (1 5 9) (3 5 7) (1 3 5 7 9)))

	(test/equal "extract-bingo-sets"
		(extract-bingo-sets 5 tbl5) '((1 2 3 4 5) (6 7 8 9 10) (11 12 13 14 15) (16 17 18 19 20) (21 22 23 24 25) (1 6 11 16 21) (2 7 12 17 22) (3 8 13 18 23) (4 9 14 19 24) (5 10 15 20 25) (1 5 21 25) (1 7 13 19 25) (5 9 13 17 21) (1 5 7 9 13 17 19 21 25)))
		
	;; (bingo-set->binomial-set bs)
	(test-define "bingo set"
		bs3 (extract-bingo-sets 3 tbl3))
		
	(test-define "bingo set"
		bs5 (extract-bingo-sets 5 tbl5))
		
	(test/equal "bingo-set->binomial-set"
		(bingo-set->binomial-set bs3) '(7 56 448 73 146 292 325 273 84 341))
	
	(test/equal "bingo-set->binomial-set"
		(bingo-set->binomial-set bs5) '(31 992 31744 1015808 32505856 1082401 2164802 4329604 8659208 17318416 17825809 17043521 1118480 18157905))
	
	;; (find-winners-set binomial-set call)
	(test/equal "find-winners-set"
		(find-winners-set '(1 2 3) (list->binomial '(1 2 3 5 8))) 2) ;; lamb: what to do for this?
		
	;; (bingo-3x3-map-stream stream bingo-table proc merge i)
	(test-define "map stream"
		st (make-number-list-stream '(1 2 3)))
		
	;; (bingo-3x3-sheet-map-stream stream bingo-sheet proc merge i)
	
)
