
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


; helper function to remove num from alist, return result list 
; if num does not exist in alist, do not make change
; if multiple num exists, just remove the first one
(defun remove-element (alist num)
	(cond ((null alist) nil)
		((= num (first alist)) (rest alist))
		(t (cons (first alist) (remove-element (rest alist) num)))
	) ; end remove
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


; ================== Implement Backtracking (basic) =================== 

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

; (defun sat? (n csp)
; 	(BT-helper '() n csp)
; )

; (defun solve-cnf (filename)
;  (let ((cnf (parse-cnf filename))) (sat? (first cnf) (second cnf))))

; (format t "test solve-cnf: ~d ~%" (solve-cnf 'cnfs/sat/cnf_10.cnf))

; ============ implement MRV and looking forward table ===============
; initialize variable selection
(defun initiate-table (n)
	(cond ((= n 1) (cons (list 1 -1) nil))
		(t (cons (list n (* n -1)) (initiate-table (- n 1))))
	)
)

; helper function to find ()/nil exists in table
(defun isvalid-table (table)
	(cond ((null table) t)
		((null (first table)) nil)
		(t (isvalid-table (rest table)))
	) 
)

; helper function to find (1) exists in table and advance (1) to first 
; helper function to advance length 1 clause to front
(defun exist-single (table)
	(cond ((null table) nil)
		((= 1 (length (first table))) t)
		(t (exist-single (rest table)))
	)
)
(defun advance-single (table)
	(cond ((= (length (first table)) 1) table)
		(t (append (advance-single (rest table)) (list (first table)))) ; bug fixed (list and append)
	)
)

; advance length = 1 clause to first if exists, 
; otherwise do not make change
(defun order-variable-table (table)
	(cond ((not (exist-single table)) table)
		(t (advance-single table))
	)
); end defun

; (1 -1) => () or (1) or (-1) or (1 -1)
(defun update-table-helper (element var csp)
	(cond ((= 1 (length element)) 
		(let ((assign (list var (car element))))
			(if (isValid assign csp)
				element
				nil
			); end if
		))
	(t (let* ((x (list (car element) var))
			(y (list (second element) var)))
		(cond ((and (isValid x csp) (isValid y csp)) element)
			((isValid x csp) (list (car element)))
			((isValid y csp) (rest element))
			(t nil)
		)); end cond; end let; 
	); end t
	); end cond
) ;end defun

; update table after var is assigned (var or -var)
; no null exists (all elements have length >= 1)
(defun update-table (table var csp)
	(cond ((null var) table)
		((null table) nil)
		(t (append (list (update-table-helper (first table) var csp)) (update-table (rest table) var csp)))
	)

)

; (format t "test update-table-helper: ~d ~%" (update-table-helper '(2 -2) 3 '((1 -2 3) (-1) (-2 -3))));  nil
; (format t "test update-table: ~d ~%" (update-table '((2 -2) (1 -1)) 3 '((1 -2 3) (-1) (-2 -3))));  nil

(defun add-assign-2 (assign var)
	(cond ((null assign) (cons var nil))
		(t (append assign (cons var nil)))
	)
)

(defun BT-helper-2 (assign n csp table)
	(cond ((is-complete assign n) assign)
		((null table) nil)
		(t (let* 
			((var1 (first (first (order-variable-table table))))
			(var2 (second (first (order-variable-table table))))
			(tt1 (update-table (rest (order-variable-table table)) var1 csp))
			(tt2 (update-table (rest (order-variable-table table)) var2 csp))
			) ; end variable assignment 
			(if (or (null var2) (not (isvalid-table tt2)))
				(cond ((and (isValid (add-assign-2 assign var1) csp) (isvalid-table tt1)) 
					(BT-helper-2 (add-assign-2 assign var1) n csp tt1)); end cond1
					(t nil)
				)
				; var1 and var2 are both valid
				(cond ((and (isValid (add-assign-2 assign var1) csp) (isvalid-table tt1))
					(let ((res (BT-helper-2 (add-assign-2 assign var1) n csp tt1)))
						(if (not (null res))
							res
							(if (isValid (add-assign-2 assign var2) csp)
								(BT-helper-2 (add-assign-2 assign var2) n csp tt2)
								nil
							); end if
						); end if
					); end let
					); end cond1
					((isValid (add-assign-2 assign var2) csp) (BT-helper-2 (add-assign-2 assign var2) n csp tt2))
					(t nil)
				); end cond
			); end if
		)) ; end let and t)
	) ; end cond
) ; end defun


(defun check-true-clause (res clause)
	(cond ((null clause) nil)
		(t (or (> (count (first clause) res) 0) (check-true-clause res (rest clause))))
	)
)

(defun check-true (res csp)
	(cond ((null csp) t)
		(t (and (check-true-clause res (first csp)) (check-true res (rest csp))))
	)
)

(defun sat? (n csp)
	(BT-helper-2 '() n csp (initiate-table n))
)

(defun solve-cnf (filename)
 (let ((cnf (parse-cnf filename))) (sat? (first cnf) (second cnf))))


; (format t "test check-true : ~d ~%" (check-true '(3 -2 -1) '((1 -2 3) (-1) (-2 -3)))) ; Nil
(format t "test solve-cnf: ~d ~%" (solve-cnf 'cnfs/sat/cnf_50.cnf))
(format t "test check true: ~d ~%" (check-true (solve-cnf 'cnfs/sat/cnf_50.cnf) (second (parse-cnf 'cnfs/sat/cnf_50.cnf))))


; (format t "test is-complete: ~d ~%" (is-complete '(1) 10)) ; Nil
; (format t "test abs-list : ~d ~%" (abs-list '(-3 2 -1))) ; (3 2 1)
; (format t "test negate-list : ~d ~%" (negate-list '(3 -1))) ; (3 2 1)
; (format t "test belong-to : ~d ~%" (belong-to '(8 7 3) '(1 3 7 8))) ; (3 2 1)
; (format t "test isValid-clause : ~d ~%" (isValid-clause '(1 2 3 4 5 6 7 8 9) '(-4 -8 -9))) ; Nil
; (format t "test valid: ~d ~%" (isValid '(-1 -2 3) '((1 -2 3) (-1) (-2 -3)))) ; Nil

; =====  BT test =====
; (format t "test select-variable: ~d ~%" (select-variable '(1) 10)) 
; (format t "test add-assign: ~d ~%" (add-assign '(1 2 3 4 5 6 7 8) 9 1))

; (format t "test isValid: ~d ~%" (isValid (add-assign '(1 2 3 4 5 6 7 8 ) 9 1) (second (parse-cnf 'cnfs/sat/cnf_10.cnf)))) 
; (format t "test BT-helper: ~d ~%" (BT-helper '() 10 (second (parse-cnf 'cnfs/sat/cnf_10.cnf)))) 

; (format t "test BT-helper 2: ~d ~%" (BT-helper '() 12 (second (parse-cnf 'cnfs/unsat/cnf_12.cnf)))) 

; (format t "test BT-helper-2: ~d ~%" (BT-helper-2 '() 50 (second (parse-cnf 'cnfs/sat/cnf_50.cnf)) (initiate-table 50))) ; Nil
; (format t "test BT-helper: ~d ~%" (BT-helper '() 50 (second (parse-cnf 'cnfs/sat/cnf_50.cnf)))) 


; (format t "test BT-helper-2: ~d ~%" (BT-helper-2 '() 12 (second (parse-cnf 'cnfs/unsat/cnf_12.cnf)) (initiate-table 12))) ; Nil
; (format t "test BT-helper: ~d ~%" (BT-helper '() 12 (second (parse-cnf 'cnfs/unsat/cnf_12.cnf)))) 

; (format t "test BT-helper: ~d ~%" (BT-helper '() 30 (second (parse-cnf 'cnfs/unsat/cnf_30.cnf)))) 
; (format t "test BT-helper-2: ~d ~%" (BT-helper-2 '() 30 (second (parse-cnf 'cnfs/unsat/cnf_30.cnf)) (initiate-table 30))) ; Nil

; (format t "test check-true 50 : ~d ~%" (check-true (BT-helper-2 '() 50 (second (parse-cnf 'cnfs/sat/cnf_50.cnf)) (initiate-table 50))
; ) (second (parse-cnf 'cnfs/sat/cnf_50.cnf))) ; Nil


