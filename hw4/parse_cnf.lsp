
(defun split-line (line)
  (if (equal line :eof)
      :eof
      (with-input-from-string (s line) (loop for x = (read s nil) while x collect x))))

(defun read-cnf (filename)
  (with-open-file (in filename)
    (loop for line = (split-line (read-line in nil :eof)) until (equal line :eof)
      if (equal 'p (first line)) collect (third line)      ; var count
      if (integerp (first line)) collect (butlast line)))) ; clause

(defun parse-cnf (filename)
  (let ((cnf (read-cnf filename))) (list (car cnf) (cdr cnf))))

; Following is a helper function that combines parse-cnf and sat?
; (defun solve-cnf (filename)
;  (let ((cnf (parse-cnf filename))) (sat? (first cnf) (second cnf))))

(format t "test read: ~d ~%" (first (parse-cnf 'cnfs/sat/cnf_10.cnf))) 



;====== simple sat without variable selection and forward checking =====
; helper function to remove num from alist, return result list 
; if num does not exist in alist, do not make change
; if multiple num exists, just remove the first one
(defun remove-element (alist num)
	(cond ((null alist) nil)
		((= num (first alist)) (rest alist))
		(t (cons (first alist) (remove-element (rest alist) num)))
	) ; end remove
)
; (format t "test remove : ~d ~%" (remove-element '(3) 3)) ;Nil
; (format t "test remove : ~d ~%" (remove-element '(1 2) 3)) ; (1 2)

(defun no-replicate (alist)
)

(defun find-min (alist)
)

(defun find-max (alist)
)

; check if assign list is complete
; length of assign equals n
; assume no replicated elements;  smallest value is 1 and largest is n
(defun is-complete (assign n)
	(cond ((null assign) nil)
		((= (length assign) n) t)
		(t nil)
	)
); end defun


; return abs of list
(defun abs-list (alist)
	(cond ((null alist) nil)
		((< (first alist) 0) (cons (* (first alist) (- 0 1)) (abs-list (rest alist))))
		(t (cons (first alist) (abs-list (rest alist))))
	)
)
; return negate of alist
(defun negate-list (alist)
	(cond ((null alist) nil)
		(t (cons (* (first alist) (- 0 1)) (negate-list (rest alist))))
	)
)

(defun abs-order (alist)
)

; every atom in short have corresponding one in long
; assume short and long do not have duplicates
; length(short) <= length(long)
(defun belong-to (short long)
	(cond ((null short) t)
		((> (count (first short) long) 0) 
			(belong-to (remove-element short (first short)) (remove-element long (first short))))
		(t nil)
	) ; end cond
) ; end defun


; helper function to check if assign is compatible with a clause
(defun isValid-clause (assign clause)
	(cond ((< (length assign) (length clause)) t)
		(t (not (belong-to clause (negate-list assign)))) 
	)
)

; check if assign breaks any constraints
; asssign
(defun isValid (assign csp)
	(cond ((null csp) t)
		(t (and (isValid-clause assign (first csp)) (isValid assign (rest csp))))
	) ; end cond
); end defun


; ====== helper function for Backtracking ===== 
(defun absolute (number)
	(cond ((< number 0) (* number -1))
		(t number)
	)
)

; get last element of a list, return atom
(defun getLast (alist)
	(cond ((null alist) nil)
		((= (length alist) 1) (car alist))
		(t (getLast (rest alist)))
	)
)

; select variable to assign value according to predefined order (1 2 3.. n)
; return number/atom 
(defun select-variable (assign n)
	(cond ((null assign) 1)
		((is-complete assign n) nil)
		(t (+ (absolute (getLast assign)) 1))
	) ; end cond
)



; helper function to return new assignment using default order
(defun add-assign (assign var tf)
	(cond ((null assign) (cons (* var tf) nil))
		(t (append assign (cons (* var tf) nil)))
	)
)

(defun BT-helper (assign n csp)
	(if (is-complete assign n) 
		assign
		(let ((var (select-variable assign n)))
			(cond ((isValid (add-assign assign var 1) csp)  
				(let ((res (BT-helper (add-assign assign var 1) n csp)))
					(if (not(null res))
						res
						(if (isValid (add-assign assign var -1) csp) 
							(BT-helper (add-assign assign var -1) n csp)
							nil
						) ; end if
					); end if 
				)); end cond1
				((isValid (add-assign assign var -1) csp) (BT-helper (add-assign assign var -1) n csp))
				(t nil)
			) ; end cond
		); end let
	); end if
)

(format t "test is-complete: ~d ~%" (is-complete '(1) 10)) ; Nil
; (format t "test abs-list : ~d ~%" (abs-list '(-3 2 -1))) ; (3 2 1)
; (format t "test negate-list : ~d ~%" (negate-list '(3 -1))) ; (3 2 1)
; (format t "test belong-to : ~d ~%" (belong-to '(8 7 3) '(1 3 7 8))) ; (3 2 1)
(format t "test isValid-clause : ~d ~%" (isValid-clause '(1 2 3 4 5 6 7 8 9) '(-4 -8 -9))) ; Nil
; (format t "test valid: ~d ~%" (isValid '(-1 -2 3) '((1 -2 3) (-1) (-2 -3)))) ; Nil

; =====  BT test =====
; (format t "test select-variable: ~d ~%" (select-variable '(1) 10)) 
(format t "test add-assign: ~d ~%" (add-assign '(1 2 3 4 5 6 7 8) 9 1))

(format t "test isValid: ~d ~%" (isValid (add-assign '(1 2 3 4 5 6 7 8 ) 9 1) (second (parse-cnf 'cnfs/sat/cnf_10.cnf)))) 
(format t "test BT-helper: ~d ~%" (BT-helper '() 10 (second (parse-cnf 'cnfs/sat/cnf_10.cnf)))) 

(format t "test BT-helper 2: ~d ~%" (BT-helper '() 12 (second (parse-cnf 'cnfs/unsat/cnf_12.cnf)))) 


