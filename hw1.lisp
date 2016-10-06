; 1. check whether N appear in an ordered tree
(defun TREE-CONTAINS (N TREE)
	; arguments: ordered trees TREE
	;            number N
	; return: t if N is in the ordered tree
	; 		  Nil otherwise
	(cond
		; base case TREE has only one element, if element == N, t else Nil
		((numberp TREE) (if (= N TREE) t Nil))
		; compare with second element
		((= N (second TREE)) t)
		((> N (second TREE)) (TREE-CONTAINS N (third TREE)))
		((< N (second TREE)) (TREE-CONTAINS N (first TREE)))
	)
)

; test case
; (format t "test 1: ~a ~%" (TREE-CONTAINS 3 '((1 2 3) 7 8)))
; (format t "test 1: ~a ~%" (TREE-CONTAINS 4 '((1 2 3) 7 8)))


; 2. return maximum number appear in ordered tree
(defun TREE-MAX (TREE)
	; argument: ordered tree
	; return: maximum number in the ordered tree
	(cond 
		; base case: TREE has only one element
		((numberp TREE) TREE)
		(t (TREE-MAX (third TREE)))
	)
)

; (format t "test 2: ~d ~%" (TREE-MAX '(1 2 3)))
; (format t "test 2: ~d ~%" (TREE-MAX '((1 2 3) 5 (6 8 (9 10 (11 12 13))))))

; 3. 
(defun TREE-ORDER (TREE)
	; argument: ordered tree
	; return: a list of numbers in  order
	(cond
		; base case TREE has only one element
		((numberp TREE) (cons TREE Nil))
		; append first second and third list () around TREE-ORDER (first TREE)
		(t (append (TREE-ORDER (first TREE)) (cons (second TREE) Nil) (TREE-ORDER (third TREE))))
	)
)

; (format t "test 3: ~d ~%" (TREE-ORDER 3))
; (format t "test 3: ~d ~%" (TREE-ORDER '((1 2 3) 7 8)))

; 4. fetch list from start and have length len
(defun SUB-LIST (L START LEN)
	; argument: 
	; 	ordered tree L
	; 	integer START: start position
	; 	integer LEN: length
	; return: a list from start and have length LEN
	(cond
		; base case: LEN = 0, return Nil
		((= LEN 0) Nil)
		; if len > 0
		((= START 0) (cons (first L) (SUB-LIST (rest L) 0 (- LEN 1))))
		; if start > 0
		((> START 0) (SUB-LIST (rest L) (- START 1) LEN))
	)
)
; test 4.0
; (format t "test 4: ~d ~%" (SUB-LIST '(a b c d) 0 3))
; (format t "test 4: ~d ~%" (SUB-LIST '(a b c d) 3 1))
; (format t "test 4: ~d ~%" (SUB-LIST '(a b c d) 2 0))

; 5. split a list
(defun SPLIT-LIST (L)
	; argument: list L
	; return: two lists L1 and L2
	; 	length of L2 - length of L1  = 0 or 1
	(let ((size (length L)))
	(cond
		; list to combine to list, cons not correct ((a b) c d) not ((a b) (c d))
		((evenp size) (list (SUB-LIST L 0 (/ size 2)) (SUB-LIST L (/ size 2) (/ size 2))))
		(t (list (SUB-LIST L 0 (/ (- size 1) 2)) (SUB-LIST L (/ (- size 1) 2) (/ (+ 1 size) 2))))
		)
	)
)
; test 5.0
; (format t "test 5: ~d ~%" (SPLIT-LIST '(a b)))
; (format t "test 5: ~d ~%" (SPLIT-LIST '(a b c d e)))
; (format t "test 5: ~d ~%" (SPLIT-LIST '(a b c d e f)))


; 6. return height of a binary tree
(defun BTREE-HEIGHT (TREE)
	; argument: binary tree 
	; return: height of binary tree
	(cond
		; single element, return 0
		((atom TREE) 0)
		(t (let ((left (BTREE-HEIGHT (first TREE))) (right (BTREE-HEIGHT (second TREE))))
			(if (< left right) (+ right 1) (+ left 1))
			)
		)
	)
)

; test 6.0
; (format t "test 6: ~d ~%" (BTREE-HEIGHT 1))
; (format t "test 6: ~d ~%" (BTREE-HEIGHT '(1 2)))
; (format t "test 6: ~d ~%" (BTREE-HEIGHT '(1 (2 3))))
; (format t "test 6: ~d ~%" (BTREE-HEIGHT '((1 2) (3 4))))

; 7. takes non-empty list and returns a binary tree
(defun LIST2BTREE (LEAVES)
	; argument: a list
	; returns: Btree format list
	(let ((size (length LEAVES)))
	(cond
		; single item or 2 item 
		((= size 1) (first LEAVES))
		((= size 2) LEAVES)
		; split list and combined the two btrees
		(t (let ((res (SPLIT-LIST LEAVES)))
			(list (LIST2BTREE (first res)) (LIST2BTREE (second res)))
			)
		)
	))
)

; test 7.0
; (format t "test 7: ~d ~%" (LIST2BTREE '(1)))
; (format t "test 7: ~d ~%" (LIST2BTREE '(1 2)))
; (format t "test 7: ~d ~%" (LIST2BTREE '(1 2 3)))
; (format t "test 7: ~d ~%" (LIST2BTREE '(1 2 3 4 5 6 7)))
; (format t "test 7: ~d ~%" (LIST2BTREE '(1 2 3 4 5 6 7 8)))


; 8. take a binary tree and return a list of atoms
(defun BTREE2LIST (TREE)
	; argument: Btree
	; return: flattened list
	(cond
		((atom TREE) (cons TREE Nil))
		(t (append (BTREE2LIST (first TREE)) (BTREE2LIST (second TREE))))
	)
)






; test 8.0
(format t "test 8: ~d ~%" (BTREE2LIST 1))
(format t "test 8: ~d ~%" (BTREE2LIST '(1 2)))
(format t "test 8: ~d ~%" (BTREE2LIST '(1 (2 3))))
(format t "test 8: ~d ~%" (BTREE2LIST '(((1 2) (3 4)) ((5 6) (7 8)))))


