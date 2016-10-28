; Homework 4
; Zhehao Wang, 404380075, zhehao@cs.ucla.edu


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
(defun solve-cnf (filename)
  (let ((cnf (parse-cnf filename))) (sat? (first cnf) (second cnf))))



(defun in-assign (assign value)
  (cond
    ((null assign) nil)
    ((= (car assign) value) t)
    ((= (+ (car assign) value) 0) nil)
    (t (in-assign (cdr assign) value))
  )
)

(defun check-clause (assign clause)
  (if (null clause)
    nil
    (if (in-assign assign (car clause))
      t
      (check-clause assign (cdr clause))
    )
  )
)

(defun check-clauses (assign delta)
  (if (null delta)
    t
    (and (check-clause assign (car delta)) (check-clauses assign (cdr delta)))
  )
)

(defun element-abs (value)
  (if (< value 0)
    (- 0 value)
    value
  )
)

(defun array-abs (array)
  (cond
    ((null array) nil)
    (t (cons (element-abs (car array)) (array-abs (cdr array))))
  )
)

(defun copy-and-reverse-at-idx (array idx)
  (cond
    ((null array) nil)
    ((= idx 1) (cons (- 0 (car array)) (array-abs (cdr array))))
    (t (cons (car array) (copy-and-reverse-at-idx (cdr array) (- idx 1))))
  )
)

(defun reverse-list (array)
  (if (null array)
    nil
    (append (reverse-list (cdr array)) (list (car array))))
)

(defun find-backtrack-point (array)
  (cond
    ((null array) 0)
    ; 0 meaning we've found the next backtrack point
    ((> (car array) 0) 0)
    ; let and if statements are added to handle the overrided semantic of the return value of this function
    (t (+ 1 (find-backtrack-point (cdr array))))
  )
)

(defun dfs (n sequence seqlen delta)
  (if (equal seqlen n) 
    (if (check-clauses sequence delta)
      sequence
      (let ((idx (find-backtrack-point (reverse sequence))))
        (if (= idx n)
          nil
          (dfs n (copy-and-reverse-at-idx sequence (- n idx)) (- n idx) delta)
        )
      )
    )
    (dfs n sequence (+ seqlen 1) delta)
  )
)

(defun default-sequence (n)
  (if (= n 0)
    nil
    (append (default-sequence (- n 1)) (list n))
  )
)

(defun sat? (n delta) 
  (dfs n (default-sequence n) 0 delta)
)
; (dfs 3 (default-sequence 3) 0 '((1 2) (-1 2) (-2)))

; (load "/Users/zhehaowang/projects/ucla-cs-2015/cs-161/homeworks/hw4/hw4.lsp")
; (solve-cnf "/Users/zhehaowang/projects/ucla-cs-2015/cs-161/homeworks/hw4/cnfs/sat/cnf_10.cnf")
; (solve-cnf "/Users/zhehaowang/projects/ucla-cs-2015/cs-161/homeworks/hw4/cnfs/unsat/cnf_20.cnf")