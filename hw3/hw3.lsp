;
; CS161 HW3: Sokoban
;
; *********************
;    READ THIS FIRST
; *********************
;
; All functions that you need to modify are marked with 'EXERCISE' in their
; header comments. This file also contains many helper functions. You may call
; any of them in your functions.
;
; Do not modify a-star.lsp.
;
; *Warning*: The provided A* code only supports the maximum cost of 4999 for any
; node. That is f(n)=g(n)+h(n) < 5000. So, be careful when you write your
; heuristic functions.  Do not make them return anything too large.
;
; For Allegro Common Lisp users: The free version of Allegro puts a limit on
; memory. So, it may crash on some hard sokoban problems and there is no easy
; fix (unless you buy Allegro). Of course, other versions of Lisp may also crash
; if the problem is too hard, but the amount of memory available will be
; relatively more relaxed. Improving the quality of the heuristic will mitigate
; this problem, as it will allow A* to solve hard problems with fewer node
; expansions. In either case, this limitation should not significantly affect
; your grade.
; 
; Remember that most functions are not graded on efficiency (only correctness).
; Efficiency can only influence your heuristic performance in the competition
; (which will affect your score).
;  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; General utility functions
; They are not necessary for this homework.
; Use/modify them for your own convenience.
;

;
; For reloading modified code.
; I found this easier than typing (load "filename") every time.
;
(defun reload ()
  (load "hw3.lsp")
  )

;
; For loading a-star.lsp.
;
(defun load-a-star ()
  (load "a-star.lsp"))

;
; Reloads hw3.lsp and a-star.lsp
;
(defun reload-all ()
  (reload)
  (load-a-star)
  )

;
; A shortcut function.
; goal-test and next-states stay the same throughout the assignment.
; So, you can just call (sokoban <init-state> #'<heuristic-name>).
; 
(defun sokoban (s h)
  (a* s #'goal-test #'next-states h)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; end general utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; We now begin actual Sokoban code
;

; Define some global variables
(setq blank 0)
(setq wall 1)
(setq box 2)
(setq keeper 3)
(setq star 4)
(setq boxstar 5)
(setq keeperstar 6)

; Some helper functions for checking the content of a square
(defun isBlank (v)
  (= v blank)
  )

(defun isWall (v)
  (= v wall)
  )

(defun isBox (v)
  (= v box)
  )

(defun isKeeper (v)
  (= v keeper)
  )

(defun isStar (v)
  (= v star)
  )

(defun isBoxStar (v)
  (= v boxstar)
  )

(defun isKeeperStar (v)
  (= v keeperstar)
  )

;
; Helper function of getKeeperPosition
;
(defun getKeeperColumn (r col)
  (cond ((null r) nil)
	(t (if (or (isKeeper (car r)) (isKeeperStar (car r)))
	       col
	     (getKeeperColumn (cdr r) (+ col 1))
	     );end if
	   );end t
	);end cond
  )

;
; getKeeperPosition (s firstRow)
; Returns a list indicating the position of the keeper (c r).
; 
; Assumes that the keeper is in row >= firstRow.
; The top row is the zeroth row.
; The first (right) column is the zeroth column.
;
(defun getKeeperPosition (s row)
  (cond ((null s) nil)
	(t (let ((x (getKeeperColumn (car s) 0)))
	     (if x
		 ;keeper is in this row
		 (list x row)
		 ;otherwise move on
		 (getKeeperPosition (cdr s) (+ row 1))
		 );end if
	       );end let
	 );end t
	);end cond
  );end defun

;
; cleanUpList (l)
; returns l with any NIL element removed.
; For example, if l is '(1 2 NIL 3 NIL), returns '(1 2 3).
;
(defun cleanUpList (L)
  (cond ((null L) nil)
	(t (let ((cur (car L))
		 (res (cleanUpList (cdr L)))
		 )
	     (if cur
		 (cons cur res)
		  res
		 )
	     );end let
	   );end t
	);end cond
  );end

; EXERCISE: Modify this function to return true (t)
; if and only if s is a goal state of a Sokoban game.
; (no box is on a non-goal square)
;
; Currently, it always returns NIL. If A* is called with
; this function as the goal testing function, A* will never
; terminate until the whole search space is exhausted.

(defun goal-test (s)
	(cond 
		; if list is empty, no 2 found, return t
		((null s) t)
		(t (and (row-not-contains-box (car s)) (goal-test (cdr s))))
	) ;end cond

  );end defun

; return t if row r contains 2
(defun row-not-contains-box (r)
	(= 0 (count box r))
)

; EXERCISE: Modify this function to return the list of
; sucessor states of s.
;
; This is the top-level next-states (successor) function.
; Some skeleton code is provided below.
; You may delete them totally, depending on your approach.
;
; If you want to use it, you will need to set 'result' to be
; the set of states after moving the keeper in each of the 4 directions.
; A pseudo-code for this is:
;
; ...
; (result (list (try-move s UP) (try-move s DOWN) (try-move s LEFT) (try-move s RIGHT)))
; ...
; 
; You will need to define the function try-move and decide how to represent UP,DOWN,LEFT,RIGHT.
; Any NIL result returned from try-move can be removed by cleanUpList.
;
(defun next-states (s)
  (let* ((pos (getKeeperPosition s 0))
	 (x (car pos))
	 (y (cadr pos))
	 ;x and y are now the coordinate of the keeper in s.
	 (result (list (try-move s 'up) (try-move s 'right) (try-move s 'down) (try-move s 'left)))
	 )
    (cleanUpList result);end
   );end let
  );

; return the integer content of position (c, r)
(defun get-square (s r c)
	(if (null s) 
		wall
		(if (= 0 r)
			(get-square-from-column (first s) c)
			(get-square (rest s) (- r 1) c)
		); end if
	); end if
) ; end defun

; helper funtion to get square content from row (list) at index col c
(defun get-square-from-column (row c)
	(if (null row)
		wall
		(if (= 0 c)
			(first row)
			(get-square-from-column (rest row) (- c 1))
		) ; end if 
	) ; end if
) ;end defun

(defun set-square (S r c v)
	(if (null S)
		nil
		(if (= r 0)
			(cons (set-square-for-row (first S) c v) (rest S))
			(cons (first S) (set-square (rest S) (- r 1) c v))
		) ; end if
	) ; end if
) ; end defun

; helper function to reset row element at colume index c
; return same value if out of bound
(defun set-square-for-row (R c v)
	(if (null R) 
		nil
		(if (= 0 c)
			(cons v (rest R))
			(cons (first R) (set-square-for-row (rest R) (- c 1) v))
		) ;end if
	) ;end if
) ; end defun


(defun try-move (S D)
	(let* ((pos (getKeeperPosition S 0))
		(c (car pos))
		(r (cadr pos)))
		(cond
			((equal D 'up) (try-move-to S c r c (- r 1) c (- r 2)))
			((equal D 'right) (try-move-to S c r (+ c 1) r (+ c 2) r))
			((equal D 'down) (try-move-to S c r c (+ r 1) c (+ r 2)))
			((equal D 'left) (try-move-to S c r (- c 1) r (- c 2) r))
			(t Nil)
		) ; end cond
	); end let*
) ;end defun

; helper funtion for move
; input original state S, original position c0, r0, 
	; possible effected positions c1, r1 and c2, r2
; return changed state if valid, Nil if invalid
(defun try-move-to (S c0 r0 c1 r1 c2 r2)
	(cond
		((isBlank (get-square S r1 c1)) (set-two-square S r0 c0 r1 c1 keeper))
		((isWall (get-square S r1 c1)) Nil)
		((isStar (get-square S r1 c1)) (set-two-square S r0 c0 r1 c1 keeperstar))
		((isBox (get-square S r1 c1))
			(let* ((pos3 (get-square S r2 c2)))
				(cond 
					((or (or (isWall pos3) (isBox pos3)) (isBoxStar pos3)) Nil)
					((isBlank pos3) (set-third-square S r0 c0 r1 c1 r2 c2 keeper box))
					((isStar pos3) (set-third-square S r0 c0 r1 c1 r2 c2 keeper boxstar))
					(t Nil)
				) ; end cond
			 ); end let 
		) ; end isbox
		((isBoxStar (get-square S r1 c1))
			(let* ((pos3 (get-square S r2 c2)))
				(cond 
					((or (or (isWall pos3) (isBox pos3)) (isBoxStar pos3)) Nil)
					((isBlank pos3) (set-third-square S r0 c0 r1 c1 r2 c2 keeperstar box))
					((isStar pos3) (set-third-square S r0 c0 r1 c1 r2 c2 keeperstar boxstar))
					(t Nil)
				) ; end cond
			 ); end let 
		) ; end isBoxStar
		(t Nil)
	) ; end cond 
)

; reuturn state after setting (c0, r0) to correct value
(defun set-first-square (S r0 c0)
	(cond 
		((isKeeper (get-square S r0 c0)) (set-square S r0 c0 blank))
		((isKeeperStar (get-square S r0 c0)) (set-square S r0 c0 star))
		(t Nil)
	) ; end cond
) ; end defun

; return state after setting two squares, 
(defun set-two-square (S r0 c0 r1 c1 p2)
	(set-square (set-first-square S r0 c0) r1 c1 p2)
) ; end defun

; return state after setting three square
(defun set-third-square (S r0 c0 r1 c1 r2 c2 p2 p3)
	(set-square (set-two-square S r0 c0 r1 c1 p2) r2 c2 p3)
) ; end defun


; EXERCISE: Modify this function to compute the trivial
; admissible heuristic.
;
; answer: this is a admissible heuristic, becasuse the steps take to put those boxes into
; goal position is larger than the number of boxes(not on goal position), assuming only one 
; agent and can only push one box
(defun h0 (s)
	0
)

; EXERCISE: Modify this function to compute the
; number of misplaced boxes in s.
;
(defun h1 (s)
	(if (null s) 
		0
		(+ (count box (first s)) (h1 (rest s)))
) ; endif


) ; end defun

; EXERCISE: Change the name of this function to h<UID> where
; <UID> is your actual student ID number. Then, modify this
; function to compute an admissible heuristic value of s.
;
; This function will be entered in the competition.
; Objective: make A* solve problems as fast as possible.
; The Lisp 'time' function can be used to measure the
; running time of a function call.
;

; first part of heuristic 
; all boxes can find nearest goal as destination
; sum of box distance
(defun min-sum-box-distance (B G)
  (cond ((null B) 0)
    (t (+ (min-box-distance (first B) G nil) (min-sum-box-distance (rest B) G)))
  ) 
) ; end defun


; helper function for min-sum-box-distance
; calculate the nearest distance between a box and G
(defun min-box-distance (b G currentMin)
	(cond
		((null G) currentMin)
		(t (let ((x (m-dist b (first G))))
			(if (null currentMin) 
				(min-box-distance b (rest G) x)
				(if (< x currentMin)
					(min-box-distance b (rest G) x)
					(min-box-distance b (rest G) currentMin)
				); end if
			); end if
			) ; end let
		) ; end t

	); end cond
) ; end defun

; helper function for absoluate and manhattan distance
(defun absolute (x)
  (if (< x 0)
    (- 0 x)
    x)
)

(defun m-dist (p1 p2)
  (+ (absolute (- (first p1) (first p2))) (absolute (- (second p1) (second p2))))
)

; helper function to get all box positions 
; return ((c1, r1), (c2, r2))
(defun getBoxes (s row)
	(cond ((null s) nil)
		(t (append (getBoxColumn (car s) row 0) (getBoxes (rest s) (+ row 1))))
	); end cond
)


(defun getBoxColumn (row r c)
	(cond ((null row) nil)
		(t (if (isBox (first row))
			(cons (list c r) (getBoxColumn (rest row) r (+ c 1)))
			(getBoxColumn (rest row) r (+ c 1))
			); end if
		)
	) ; end cond
)


; helper function to get all goal positions >= row
; return ((c1, r1), (c2, r2))
(defun getGoals (s row)
	(cond ((null s) nil)
		(t (append (getGoalsColumn (car s) row 0) (getGoals (rest s) (+ row 1))))
	); end cond
) ; end defun

; helper function for getGoals
; return all goals in one row as a list ((c1, r), (c2, r)), with column > c
(defun getGoalsColumn (row r c)
	(cond ((null row) nil)
		(t (if (or (isStar (first row)) (isKeeperStar (first row)))
			(cons (list c r) (getGoalsColumn (rest row) r (+ c 1)))
			(getGoalsColumn (rest row) r (+ c 1))
			); end if
		)
	) ; end cond
)



(defun h003975861 (s)
  (min-sum-box-distance (getBoxes s 0) (getGoals s 0))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Some predefined problems.  Each problem can be visualized by calling
 | (printstate <problem>).  Problems are ordered roughly by their difficulties.
 | For most problems, we also privide 2 additional number per problem: 1) # of
 | nodes expanded by A* using our next-states and h0 heuristic.  2) the depth of
 | the optimal solution.  These numbers are located at the comments of the
 | problems. For example, the first problem below was solved by 80 nodes
 | expansion of A* and its optimal solution depth is 7.
 |
 | Your implementation may not result in the same number of nodes expanded, but
 | it should probably give something in the same ballpark. As for the solution
 | depth, any admissible heuristic must make A* return an optimal solution. So,
 | the depths of the optimal solutions provided could be used for checking
 | whether your heuristic is admissible.
 |
 | Warning: some problems toward the end are quite hard and could be impossible
 | to solve without a good heuristic!
 |
 |#

;(80,7)
(setq p1 '((1 1 1 1 1 1)
	   (1 0 3 0 0 1)
	   (1 0 2 0 0 1)
	   (1 1 0 1 1 1)
	   (1 0 0 0 0 1)
	   (1 0 0 0 4 1)
	   (1 1 1 1 1 1)))

;(110,10)
(setq p2 '((1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 1) 
	   (1 0 0 0 0 0 1) 
	   (1 0 0 2 1 4 1) 
	   (1 3 0 0 1 0 1)
	   (1 1 1 1 1 1 1)))

;(211,12)
(setq p3 '((1 1 1 1 1 1 1 1 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 2 0 3 4 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 1 1 1 1 1 1 1 1)))

;(300,13)
(setq p4 '((1 1 1 1 1 1 1)
	   (0 0 0 0 0 1 4)
	   (0 0 0 0 0 0 0)
	   (0 0 1 1 1 0 0)
	   (0 0 1 0 0 0 0)
	   (0 2 1 0 0 0 0)
	   (0 3 1 0 0 0 0)))

;(551,10)
(setq p5 '((1 1 1 1 1 1)
	   (1 1 0 0 1 1)
	   (1 0 0 0 0 1)
	   (1 4 2 2 4 1)
	   (1 0 0 0 0 1)
	   (1 1 3 1 1 1)
	   (1 1 1 1 1 1)))

;(722,12)
(setq p6 '((1 1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 4 1)
	   (1 0 0 0 2 2 3 1)
	   (1 0 0 1 0 0 4 1)
	   (1 1 1 1 1 1 1 1)))

;(1738,50)
(setq p7 '((1 1 1 1 1 1 1 1 1 1)
	   (0 0 1 1 1 1 0 0 0 3)
	   (0 0 0 0 0 1 0 0 0 0)
	   (0 0 0 0 0 1 0 0 1 0)
	   (0 0 1 0 0 1 0 0 1 0)
	   (0 2 1 0 0 0 0 0 1 0)
	   (0 0 1 0 0 0 0 0 1 4)))

;(1763,22)
(setq p8 '((1 1 1 1 1 1)
	   (1 4 0 0 4 1)
	   (1 0 2 2 0 1)
	   (1 2 0 1 0 1)
	   (1 3 0 0 4 1)
	   (1 1 1 1 1 1)))

;(1806,41)
(setq p9 '((1 1 1 1 1 1 1 1 1) 
	   (1 1 1 0 0 1 1 1 1) 
	   (1 0 0 0 0 0 2 0 1) 
	   (1 0 1 0 0 1 2 0 1) 
	   (1 0 4 0 4 1 3 0 1) 
	   (1 1 1 1 1 1 1 1 1)))

;(10082,51)
(setq p10 '((1 1 1 1 1 0 0)
	    (1 0 0 0 1 1 0)
	    (1 3 2 0 0 1 1)
	    (1 1 0 2 0 0 1)
	    (0 1 1 0 2 0 1)
	    (0 0 1 1 0 0 1)
	    (0 0 0 1 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 1 1)))

;(16517,48)
(setq p11 '((1 1 1 1 1 1 1)
	    (1 4 0 0 0 4 1)
	    (1 0 2 2 1 0 1)
	    (1 0 2 0 1 3 1)
	    (1 1 2 0 1 0 1)
	    (1 4 0 0 4 0 1)
	    (1 1 1 1 1 1 1)))

;(22035,38)
(setq p12 '((0 0 0 0 1 1 1 1 1 0 0 0)
	    (1 1 1 1 1 0 0 0 1 1 1 1)
	    (1 0 0 0 2 0 0 0 0 0 0 1)
	    (1 3 0 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 2 1 1 1 0 0 0 1)
	    (1 0 0 0 0 1 0 1 4 0 4 1)
	    (1 1 1 1 1 1 0 1 1 1 1 1)))

;(26905,28)
(setq p13 '((1 1 1 1 1 1 1 1 1 1)
	    (1 4 0 0 0 0 0 2 0 1)
	    (1 0 2 0 0 0 0 0 4 1)
	    (1 0 3 0 0 0 0 0 2 1)
	    (1 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 0 0 0 0 4 1)
	    (1 1 1 1 1 1 1 1 1 1)))

;(41715,53)
(setq p14 '((0 0 1 0 0 0 0)
	    (0 2 1 4 0 0 0)
	    (0 2 0 4 0 0 0)
	    (3 2 1 1 1 0 0)
	    (0 0 1 4 0 0 0)))

;(48695,44)
(setq p15 '((1 1 1 1 1 1 1)
	    (1 0 0 0 0 0 1)
	    (1 0 0 2 2 0 1)
	    (1 0 2 0 2 3 1)
	    (1 4 4 1 1 1 1)
	    (1 4 4 1 0 0 0)
	    (1 1 1 1 0 0 0)
	    ))

;(91344,111)
(setq p16 '((1 1 1 1 1 0 0 0)
	    (1 0 0 0 1 0 0 0)
	    (1 2 1 0 1 1 1 1)
	    (1 4 0 0 0 0 0 1)
	    (1 0 0 5 0 5 0 1)
	    (1 0 5 0 1 0 1 1)
	    (1 1 1 0 3 0 1 0)
	    (0 0 1 1 1 1 1 0)))

;(3301278,76)
(setq p17 '((1 1 1 1 1 1 1 1 1 1)
	    (1 3 0 0 1 0 0 0 4 1)
	    (1 0 2 0 2 0 0 4 4 1)
	    (1 0 2 2 2 1 1 4 4 1)
	    (1 0 0 0 0 1 1 4 4 1)
	    (1 1 1 1 1 1 0 0 0 0)))

;(??,25)
(setq p18 '((0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 4 1 0 0 0 0)
	    (0 0 0 0 1 0 2 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)
	    ))
;(??,21)
(setq p19 '((0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 0 1 0 0 1 0 0 0 0)
	    (0 0 0 0 0 0 3 0 0 0 2 0)
	    (0 0 0 0 1 0 0 1 0 0 0 4)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 2 0 4 1 0 0 0)))

;(??,??)
(setq p20 '((0 0 0 1 1 1 1 0 0)
	    (1 1 1 1 0 0 1 1 0)
	    (1 0 0 0 2 0 0 1 0)
	    (1 0 0 5 5 5 0 1 0)
	    (1 0 0 4 0 4 0 1 1)
	    (1 1 0 5 0 5 0 0 1)
	    (0 1 1 5 5 5 0 0 1)
	    (0 0 1 0 2 0 1 1 1)
	    (0 0 1 0 3 0 1 0 0)
	    (0 0 1 1 1 1 1 0 0)))

;(??,??)
(setq p21 '((0 0 1 1 1 1 1 1 1 0)
	    (1 1 1 0 0 1 1 1 1 0)
	    (1 0 0 2 0 0 0 1 1 0)
	    (1 3 2 0 2 0 0 0 1 0)
	    (1 1 0 2 0 2 0 0 1 0)
	    (0 1 1 0 2 0 2 0 1 0)
	    (0 0 1 1 0 2 0 0 1 0)
	    (0 0 0 1 1 1 1 0 1 0)
	    (0 0 0 0 1 4 1 0 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 0 1 4 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 1 1 1 1 1)))

;(??,??)
(setq p22 '((0 0 0 0 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 1 2 0 0 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 1 1 1 0 0 2 1 1 0 0 0 0 0 0 0 0 0)
	    (0 0 1 0 0 2 0 2 0 1 0 0 0 0 0 0 0 0 0)
	    (1 1 1 0 1 0 1 1 0 1 0 0 0 1 1 1 1 1 1)
	    (1 0 0 0 1 0 1 1 0 1 1 1 1 1 0 0 4 4 1)
	    (1 0 2 0 0 2 0 0 0 0 0 0 0 0 0 0 4 4 1)
	    (1 1 1 1 1 0 1 1 1 0 1 3 1 1 0 0 4 4 1)
	    (0 0 0 0 1 0 0 0 0 0 1 1 1 1 1 1 1 1 1)
	    (0 0 0 0 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Utility functions for printing states and moves.
 | You do not need to understand any of the functions below this point.
 |#

;
; Helper function of prettyMoves
; from s1 --> s2
;
(defun detectDiff (s1 s2)
  (let* ((k1 (getKeeperPosition s1 0))
	 (k2 (getKeeperPosition s2 0))
	 (deltaX (- (car k2) (car k1)))
	 (deltaY (- (cadr k2) (cadr k1)))
	 )
    (cond ((= deltaX 0) (if (> deltaY 0) 'DOWN 'UP))
	  (t (if (> deltaX 0) 'RIGHT 'LEFT))
	  );end cond
    );end let
  );end defun

;
; Translates a list of states into a list of moves.
; Usage: (prettyMoves (a* <problem> #'goal-test #'next-states #'heuristic))
;
(defun prettyMoves (m)
  (cond ((null m) nil)
	((= 1 (length m)) (list 'END))
	(t (cons (detectDiff (car m) (cadr m)) (prettyMoves (cdr m))))
	);end cond
  );

;
; Print the content of the square to stdout.
;
(defun printSquare (s)
  (cond ((= s blank) (format t " "))
	((= s wall) (format t "#"))
	((= s box) (format t "$"))
	((= s keeper) (format t "@"))
	((= s star) (format t "."))
	((= s boxstar) (format t "*"))
	((= s keeperstar) (format t "+"))
	(t (format t "|"))
	);end cond
  )

;
; Print a row
;
(defun printRow (r)
  (dolist (cur r)
    (printSquare cur)
    )
  );

;
; Print a state
;
(defun printState (s)
  (progn
    (dolist (cur s)
      (printRow cur)
      (format t "~%")
      )
    );end progn
  )

;
; Print a list of states with delay.
;
(defun printStates (sl delay)
  (dolist (cur sl)
    (printState cur)
    (sleep delay)
    );end dolist
  );end defun


; =======  test functions =======
; (load "/Users/zhehaowang/projects/ucla-cs-2015/cs-161/homeworks/hw3/a-star.lsp")
; (load "/Users/zhehaowang/projects/ucla-cs-2015/cs-161/homeworks/hw3/hw3.lsp")
; test goal-test
; (reload-all)
(load-a-star)
; (format t "test goal-test: ~d ~%" (goal-test p1)) 
; (format t "get-square-from-column: ~d ~%" (get-square-from-column '(1 0 2 0 0 1) 0))
; (format t "get-square-from-column: ~d ~%" (get-square-from-column '(1 0 2 0 0 1) 2))
; (format t "get-square-from-column: ~d ~%" (get-square-from-column '(1 0 2 0 0 1) 6))
; (format t "get-square: ~d ~%" (get-square p1 1 2))
; (format t "get-square: ~d ~%" (get-square p1 9 10))

; (format t "set-square-for-row: ~d ~%" (set-square-for-row '(1 0 2 0 0 1) 0 5))
; (format t "set-square: ~d ~%" (set-square p1 0 7 0))
; (printState p1)

; (format t "set-first-square: ~d ~%" (printState (set-first-square p1 1 2)))
; (format t "set-two-square: ~d ~%" (printState (set-two-square p1 1 2 2 2 keeperstar)))
; (format t "set-two-square: ~d ~%" (printState (set-two-square p1 1 2 1 3 keeper)))
; (printState (set-third-square p1 1 2 2 2 3 2 keeperstar blank))
; (format t "nil ~d ~%" (try-move-to p1 2 1 2 0 2 -1))
; (printState (try-move p1 'down))

; (printState p21)
; (printStates (next-states (first (next-states p5))) 0.1)
; (format t "h0 test: ~d ~%" (h0 p5))
; (format t "h1 test: ~d ~%" (h1 p5))

; (format t "getGoalsColumn: ~d ~%" (getGoalsColumn (nth 9 p21) 9 0))
; (format t "getGoals: ~d ~%" (getGoals p21 0))
; (format t "getBoxes: ~d ~%" (getBoxes p21 0))
; (format t "min-box-distance: ~d ~%" (min-box-distance (first (getBoxes p21 0)) (getGoals p21 0) nil))
; (format t "min-sum-box-distance: ~d ~%" (min-sum-box-distance (getBoxes p21 0) (getGoals p21 0)))
; (format t "heuristic: ~d ~%" (h003975861 p21))

; (printstates (a* p1 #'goal-test #'next-states #'h0) 0.2)