;
; CS161 Hw3: Sokoban
; 
; *********************
;    READ THIS FIRST
; ********************* 
;
; All functions that you need to modify are marked with 'EXERCISE' in their header comments.
; Do not modify a-star.lsp.
; This file also contains many helper functions. You may call any of them in your functions.
;
; *Warning*: The provided A* code only supports the maximum cost of 4999 for any node.
; That is f(n)=g(n)+h(n) < 5000. So, be careful when you write your heuristic functions.
; Do not make them return anything too large.
;
; For Allegro Common Lisp users: The free version of Allegro puts a limit on memory.
; So, it may crash on some hard sokoban problems and there is no easy fix (unless you buy 
; Allegro). 
; Of course, other versions of Lisp may also crash if the problem is too hard, but the amount
; of memory available will be relatively more relaxed.
; Improving the quality of the heuristic will mitigate this problem, as it will allow A* to
; solve hard problems with fewer node expansions.
; 
; In either case, this limitation should not significantly affect your grade.
; 
; Remember that most functions are not graded on efficiency (only correctness).
; Efficiency can only influence your heuristic performance in the competition (which will
; affect your score).
;  
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
  (load "hw3.lsp"))
  
(defun test ()
	(load "hw3tests.lsp"))

(defun load-games ()
	(load "hw3games.lsp"))

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
;
(defun sokoban (s h)
  (a* s #'goal-test #'next-states h))

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
  
(defun isAnyBox (v)
	(or (= v box) (= v boxstar))
)

;
; Helper function of getKeeperPosition
;
(defun getKeeperColumn (r col)
  (cond ((null r) nil)
	(t (if (or (isKeeper (car r)) (isKeeperStar (car r)))
	       (list col (isKeeperStar (car r)))
	     (getKeeperColumn (cdr r) (+ col 1))
	     );end if
	   );end t
	);end cond
  )

;
; getKeeperPosition (s firstRow)
; Returns a list indicating the position of the keeper (c r star).
; 
; Assumes that the keeper is in row >= firstRow.
; The top row is the zeroth row.
; The first (left) column is the zeroth column.
; star is T if the keeper is standing on a goal spot, NIL otherwise.
;
(defun getKeeperPosition (s row)
  (cond ((null s) nil)
	(t (let* ((x (getKeeperColumn (car s) 0)) (c (car x)) (wasstar (cadr x)))
			(if c
				;keeper is in this row
				(list c row wasstar)
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
 
;
; countListContents (lst elem cnt)
; Counts the number of times that elem appears in a list lst and adds it cnt.
;
; lst is a list (single level) and elem is an integer. Elements are compared using (=).
; cnt is a running sum of the number of times elem has already appeared in the list.
; Helper function for countStateContents.
;
(defun countListContents (lst elem cnt)
	(cond
		((null lst) cnt)
		((= (car lst) elem) (countListContents (cdr lst) elem (+ 1 cnt)))
		(t (countListContents (cdr lst) elem cnt))
	)
)

;
; countStateContents (s elem cnt)
; Counts the number of times that elem appears in the state s and adds it to cnt.
;
; s is assumed to be a valid state (a list of lists) and elem is an integer. 
; Elements are compared using (=). cnt is a running sum of the number of times 
; elem has already appeared in the state (to allow for tail call recursion if present).
; Helper function for goal-test.
;
(defun countStateContents (s elem cnt)
	(cond
		((null s) cnt)
		(t (countStateContents (cdr s) elem (+ cnt (count elem (car s)))))
		;(t (countStateContents (cdr s) elem (countListContents (car s) elem cnt)))
	)
)

; EXERCISE: Modify this function to return true (t)
; if and only if s is a goal state of the game.
; (neither any boxes nor the keeper is on a non-goal square)
;
; Currently, it always returns NIL. If A* is called with
; this function as the goal testing function, A* will never
; terminate until the whole search space is exhausted.
;
(defun goal-test (s)
  (and (= 0 (countStateContents s box 0)) (= 0 (countStateContents s keeper 0)))
  );end defun

;
; getColumnSeq (s col row len hist)
; Returns a reversed subsequence from column index col (of the two-layer list s), starting at 
; index start and of length len, appended to hist.
;
; s is assumed to be a valid state (e.g. a two-layer list where each row is the same length).
; The left-most column is column zero, and the first element of that column is index 0.
; If a subsequence extends beyond the end of the column, the elements will be nill
; (i.e. the length of the returned list is guaranteed to be length of hist + len).
; Similarly if start is less than zero.
;
(defun getColumnSeq (s col start len hist)
	(cond
		((= len 0) hist)
		((< start 0) (getColumnSeq s col (+ start 1) (- len 1) (cons nil hist)))
		((> start 0) (getColumnSeq (cdr s) col (- start 1) len hist))
		(T (getColumnSeq (cdr s) col start (- len 1) (cons (car (nthcdr col (car s))) hist)))
	)
)

;
; getSubSeq (lst start len hist)
; Gets a reversed subsequence from lst starting at start of length len, appended to hist.
;
; Hist is assumed to be a valid list. The first element is element zero. 
; If len extends beyond the end of lst, remaining elements will be null (i.e. the length of 
; the returned list is guaranteed to be length of hist + len). Simiarly if start < 0.
; Helper function for getRowSeq.
;
(defun getSubSeq (lst start len hist)
	(cond
		((= len 0) hist)
		((< start 0) (getSubSeq lst (+ start 1) (- len 1) (cons nil hist)))
		((> start 0) (getSubSeq (cdr lst) (- start 1) len hist))
		(T (getSubSeq (cdr lst) start (- len 1) (cons (car lst) hist)))
	)
)

;
; getRowSeq (s row start len hist)
; Gets a reversed subsequence from row index row (of the two-layer list s), starting at index 
; start and of length len, appended to hist.
;
; s is assumed to be a valid state (e.g. a two-layer list). The first row is row 0, and the 
; first element is zero. If a subsequence extends beyond the end of a row, remaining elements 
; will be null (i.e. the length of the returned list is guaranteed to be length of hist + len).
; Similarly for start < 0.
;
(defun getRowSeq (s row start len hist)
	(getSubSeq (car (nthcdr row s)) start len hist)
)

;
; replaceSubSeq (lst start repl)
; Replace the subsequence in lst starting at start with repl. If repl is longer than lst, only
; the elements in lst will be replaced.
;
; Negative starts are permitted, in which case the first elements of repl will be omitted.
; Helper function for replaceColSeq and replaceRowSeq.
;
(defun replaceSubSeq (lst start repl)
	(cond
		((null repl) lst)
		((null lst) lst)
		((< start 0) (replaceSubSeq lst (+ start 1) (cdr repl)))
		((> start 0) (cons (car lst) (replaceSubSeq (cdr lst) (- start 1) repl)))
		(T (cons (car repl) (replaceSubSeq (cdr lst) start (cdr repl))))
	)
)

;
; replaceColSeq (s col start repl)
; Replaces the subsequence in column index col of state s starting at row index start with
; the contents of repl. If repl is longer than the column, only the elements in the column 
; will be replaced.
;
; Negative starts are permitted, in which case the first elements of repl will be omitted.
; Helper function for try-move.
;
(defun replaceColSeq (s col start repl)
	(cond
		((null repl) s)
		((null s) s)
		((< start 0) (replaceColSeq s col (+ start 1) (cdr repl)))
		((> start 0) (cons (car s) (replaceColSeq (cdr s) col (- start 1) repl)))
		(T (cons (replaceSubSeq (car s) col (list (car repl))) (replaceColSeq (cdr s) col start (cdr repl))))
	)
)

;
; replaceRowSeq (s row start repl)
; Replaces the subsequence in row index row of state s starting at index start with the contents
; of repl. If repl is longer than the row, only the elements in the row will be replaced.
;
; Negative starts are permitted, in which case the first elements of repl will be omitted.
; Helper function for try-move.
;
(defun replaceRowSeq (s row start repl)
	(cond
		((null s) s)
		((> row 0) (cons (car s) (replaceRowSeq (cdr s) (- row 1) start repl)))
		(T (cons (replaceSubSeq (car s) start repl) (cdr s)))
	)
)

;
; validateMove (s)
; Checks if a valid move can be performed, and if so, returns the updated state after movement.
;
; s is a two-element list of the two square states in the direction of movement. If the move
; is valid, a two-element list is returned (possibly with NIL values).
; If the move is invalid, NIL is returned.
; Helper function for try-move.
;
(defun validateMove (s)
	(let ((s1 (car s))
		(s2 (cadr s)))
		(cond
			((null s1) nil) ; We're out of bounds
			((isWall s1) nil) ; Can't move into a wall
			((isStar s1) (list keeperstar s2))
			((isBlank s1) (list keeper s2))
			(T (cond ; s1 must be a box or boxstar
				((or (null s2) (isWall s2) (isBox s2) (isBoxStar s2)) NIL) ; Can't push box into those
				((and (isBox s1) (isBlank s2)) (list keeper box))
				((and (isBoxStar s1) (isBlank s2)) (list keeperstar box))
				((and (isBox s1) (isStar s2)) (list keeper boxstar))
				(T (list keeperstar boxstar))
			))
		)
	)
)

;
; swap (s)
; Swaps the two-element list s, so the first element becomes the second and vice-versa.
;
; Helper function of getDirection and try-move.
;
(defun swap (s)
	(list (cadr s) (car s))
)

;
; getDirection (s x y dir)
; Gets the two squares starting from (x, y) in the direction of dir
;
; dir 0 is left, 1 is up, 2 is right, and 3 is down
; Helper function for tryMove.
;
(defun getDirection (s x y dir)
	(cond
		((= dir 0) (getRowSeq s y (- x 2) 2 nil))
		((= dir 1) (getColumnSeq s x (- y 2) 2 nil))
		((= dir 2) (swap (getRowSeq s y (+ x 1) 2 nil)))
		(T (swap (getColumnSeq s x (+ y 1) 2 nil)))
	)
)

(defun try-move (s x y dir wasstar)
	(let ((newState (validateMove (getDirection s x y dir))))
		(cond
			((null newState) nil)
			(T (let ((keepersquare (if wasstar star blank)))
				(cond
					((= dir 0) (replaceRowSeq s y (- x 2) (append (swap newState) (list keepersquare))))
					((= dir 1) (replaceColSeq s x (- y 2) (append (swap newState) (list keepersquare))))
					((= dir 2) (replaceRowSeq s y x (cons keepersquare newState)))
					(T (replaceColSeq s x y (cons keepersquare newState)))
				)
			))
		)
	)
)

; use length to get the length of the list as bounds

; EXERCISE: Modify this function to return the list of 
; successor states of s.
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
;
(defun next-states (s)
  (let* ((pos (getKeeperPosition s 0))
	 (x (car pos))
	 (y (cadr pos))
	 ;x and y are now the coordinate of the keeper in s.
	 (wasstar (caddr pos))
	 (result (list (try-move s x y 1 wasstar) (try-move s x y 2 wasstar) (try-move s x y 3 wasstar) (try-move s x y 0 wasstar)))
	 )
    (cleanUpList result);end
   );end let
  );

; EXERCISE: Modify this function to compute the trivial 
; admissible heuristic.
;
(defun h0 (s)
	0
)

; EXERCISE: Modify this function to compute the 
; number of misplaced boxes in s.
;
(defun h1 (s)
	; This is an admissable heuristic. Every box that is not on a goal must be moved at least
	; once, therefore it will require at least that many steps to solve the puzzle, and never
	; more than that many steps since only one box can be moved at a time.
	(countStateContents s box 0)
)

;
; findListContents (lst x y boxes stars)
; Finds the coordinates of every box and star in a list lst and adds it the lists boxes and stars. 
; x and y are the coordinates of the first element in the list.
;
; lst is a list (single level) and elem is an integer. Elements are compared using (=).
; boxes is a running list of the coordinates of previous boxes in the list, and similarly for stars.
; Helper function for findStateContents.
;
(defun findListContents (lst x y boxes stars)
	(cond
		((null lst) (list boxes stars))
		((= (car lst) box) (findListContents (cdr lst) (+ x 1) y (cons (list x y) boxes) stars))
		((= (car lst) star) (findListContents (cdr lst) (+ x 1) y boxes (cons (list x y) stars)))
		(t (findListContents (cdr lst) (+ x 1) y boxes stars))
	)
)

;
; findStateContents (s y boxes stars)
; Finds the coordinates of every box and star that appears in the state s and adds it to the lists boxes
; and stars. y is the (row) index of the first element in the state.
;
; s is assumed to be a valid state (a list of lists) and elem is an integer. 
; Elements are compared using (=). boxes is a running list of the coordinates of previous boxes
; in the state, and similarly for stars (to allow for tail call recursion if present).
; Helper function for h2.
;
(defun findStateContents (s y boxes stars)
	(cond
		((null s) (list boxes stars))
		(t (let* ((rowResults (findListContents (car s) 0 y boxes stars))
				(nboxes (car rowResults))
				(nstars (cadr rowResults)))
			(findStateContents (cdr s) (+ y 1) nboxes nstars))
		)
	)
)

;
; get-square (s r c)
; Gets the square in s located at row r and column c. If (c r) is out of bounds, returns a wall instead.
;
; Helper function for is-deadlocked.
;
(defun get-square (s r c)
	(cond
		((< r 0) wall)
		((< c 0) wall)
		(T (let ((sq (car (nthcdr c (car (nthcdr r s))))))
			(if (null sq) wall sq)
			)
		)
	)
)

;
; set-element (lst i elem)
; Returns a copy of lst with element i equal to elem. If i is out of bounds, returns lst.
;
; Helper function for set-square.
;
(defun set-element (lst i elem)
	(cond
		((null lst) lst)
		((> i 0) (cons (car lst) (set-element (cdr lst) (- i 1) elem)))
		(T (cons elem (cdr lst)))
	)
)

;
; set-square (s r c elem)
; Returns a copy of s with the element at (c r) equal to elem. If (c r) is out of bounds, returns s.
;
; Helper function for is-deadlocked.
;
(defun set-square (s r c elem)
	(cond
		((null s) s)
		((< r 0) s)
		((< c 0) s)
		((> r 0) (cons (car s) (set-square (cdr s) (- r 1) c elem)))
		(T (cons (set-element (car s) c elem) (cdr s)))
	)
)

;
; is-deadlocked-axis (s p axis)
; Checks if the point at p in s is deadlocked along axis (0 = horizontal, 1 = vertical). A box is
; deadlocked if there are walls or deadlocked boxes vertically or horizontally.
;
; Helper function for sum-min-dist.
;
(defun is-deadlocked-axis (s p axis)
	(let ((x (car p)) (y (cadr p)))
		(if (= axis 0) ; 0 is the x axis
			(let ((left (get-square s y (- x 1)))
				(right (get-square s y (+ x 1))))
				(or (isWall left) (isWall right)
					(and (isAnyBox left) (is-deadlocked-axis (set-square s y x wall) (list (- x 1) y) 1)) ; Need to update state
					(and (isAnyBox right) (is-deadlocked-axis (set-square s y x wall) (list (+ x 1) y) 1))
				)
			)
			(let ((down (get-square s (- y 1) x))
				(up (get-square s (+ y 1) x)))
				(or (isWall down) (isWall up) 
					(and (isAnyBox down) (is-deadlocked-axis (set-square s y x wall) (list x (- y 1)) 0))
					(and (isAnyBox up) (is-deadlocked-axis (set-square s y x wall) (list x (+ y 1)) 0))
				)
			)
		)
	)
)

;
; is-deadlocked-2 (s p)
; Convenience function to check both axes with is-deadlocked-axis. Alternative to is-deadlocked, which
; checks for deadlock by walls and other boxes.
;
; Helper function for sum-min-dist.
;
(defun is-deadlocked-2 (s p)
	(and (is-deadlocked-axis s p 0) (is-deadlocked-axis s p 1))
)

;
; is-deadlocked (s p)
; Checks if the point at p is deadlocked in the "corner" of walls, e.g has one wall vertical and one
; horizontally.
;
; Helper function for sum-min-dist.
;
(defun is-deadlocked (s p)
	(let* ((x (car p))
		(y (cadr p))
		(left (get-square s y (- x 1)))
		(up (get-square s (- y 1) x))
		(right (get-square s y (+ x 1)))
		(down (get-square s (+ y 1) x))
		(vert (or (isWall up) (isWall down)))
		(horiz (or (isWall left) (isWall right))))
		(and vert horiz)
	)
)

;
; difference (s1 s2)
; Calculates the manhattan distance between two dimensional points s1 and s2
;
; Helper function for min-dist and sum-min-dist.
;
(defun distance (s1 s2)
	(+ (abs (- (car s1) (car s2))) (abs (- (cadr s1) (cadr s2))))
)

;
; min-dist (point targets rmin)
; Gets the minimum distance from 2D point to any of the 2D points in targets. Rmin is the running min.
;
; Be careful not to set rmin to too small a value, as that will effectively floor out the minimum.
; Helper function for sum-min-dist.
;
(defun min-dist (point targets rmin)
	(cond
		((null targets) rmin)
		(T (min-dist point (cdr targets) (min rmin (distance point (car targets)))))
	)
)

;
; sum-min-dist (points targets sum-dist)
; Sums the minimum distance from all the 2D points in points to any of the 2D points in targets
; (repetitions allowed - i.e. not one-to-one mapping). If any of the points is deadlocked by walls (and is
; unmoveable), returns 3500 instead.
;
; sum-dist is the running sum of the minimum distances so far.
; Helper function for h2.
;
(defun sum-min-dist (points targets s sum-dist)
	(cond
		((null points) sum-dist)
		;((is-deadlocked s (car points)) 3500)
		((is-deadlocked-2 s (car points)) 3500)
		(T (sum-min-dist (cdr points) targets s (+ sum-dist (min-dist (car points) (cdr targets) (distance (car points) (car targets))))))
	)
)

; EXERCISE: Modify this h2 function to compute an
; admissible heuristic value of s. 
; 
; This function will be entered in the competition.
; Objective: make A* solve problems as fast as possible.
; The Lisp 'time' function can be used to measure the 
; running time of a function call.
;
(defun h2 (s)
	; This heuristic counts the manhattan distance from every unmatched box to the nearest unoccupied
	; goal. This is an admissable heuristic, since that is the minimum number of steps for every box
	; to reach a goal.
	(let* ((counts (findStateContents s 0 nil nil))
		(boxes (car counts))
		(goals (cadr counts)))
		(sum-min-dist boxes goals s 0)
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Some predefined problems.
 | Each problem can be visualized by calling (printstate <problem>). For example, (printstate p1).
 | Problems are ordered roughly by their difficulties.
 | For most problems, we also provide a number which indicates the depth of the optimal solution.
 | These numbers are located at the comments of the problems. For example, the first problem below has optimal solution depth 6.
 | As for the solution depth, any admissible heuristic must make A* return an optimal solution. So, the depths of the optimal solutions provided could be used for checking whether your heuristic is admissible.
 |
 | Warning: some problems toward the end are quite hard and could be impossible to solve without a good heuristic!
 | 
 |#
;(6)

(setq p1 '((1 1 1 1 1 1)
	   (1 0 3 0 0 1)
	   (1 0 2 0 0 1)
	   (1 1 0 1 1 1)
	   (1 0 0 0 0 1)
	   (1 0 4 0 4 1)
	   (1 1 1 1 1 1)))

;(15)

(setq p2 '((1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 1) 
	   (1 0 0 0 0 0 1) 
	   (1 0 0 2 1 4 1) 
	   (1 3 4 0 1 0 1)
	   (1 1 1 1 1 1 1)))

;(13)
(setq p3 '((1 1 1 1 1 1 1 1 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 2 0 3 4 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 4 0 1 0 0 0 1)
	   (1 1 1 1 1 1 1 1 1)))

;(17)
(setq p4 '((1 1 1 1 1 1 1)
	   (0 0 0 0 0 1 4)
	   (0 0 0 0 0 0 0)
	   (0 0 1 1 1 0 0)
	   (0 0 1 0 0 0 0)
	   (0 2 1 0 0 4 0)
	   (0 3 1 0 0 0 0)))

;(12)
(setq p5 '((1 1 1 1 1 1)
	   (1 1 0 0 1 1)
	   (1 0 0 0 0 1)
	   (1 4 2 2 4 1)
	   (1 0 0 0 4 1)
	   (1 1 3 1 1 1)
	   (1 1 1 1 1 1)))

;(13)
(setq p6 '((1 1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 4 1)
	   (1 4 0 0 2 2 3 1)
	   (1 0 0 1 0 0 4 1)
	   (1 1 1 1 1 1 1 1)))

;(47)
(setq p7 '((1 1 1 1 1 1 1 1 1 1)
	   (0 0 1 1 1 1 4 0 0 3)
	   (0 0 0 0 0 1 0 0 0 0)
	   (0 0 0 0 0 1 0 0 1 0)
	   (0 0 1 0 0 1 0 0 1 0)
	   (0 2 1 0 0 0 0 0 1 0)
	   (0 0 1 0 0 0 0 0 1 4)))

;(22)
(setq p8 '((1 1 1 1 1 1)
	   (1 4 0 0 4 1)
	   (1 0 2 2 0 1)
	   (1 2 0 1 0 1)
	   (1 3 4 0 4 1)
	   (1 1 1 1 1 1)))

;(34)
(setq p9 '((1 1 1 1 1 1 1 1 1) 
	   (1 1 1 0 0 1 1 1 1) 
	   (1 0 0 0 0 0 2 0 1) 
	   (1 0 1 0 0 1 2 0 1) 
	   (1 0 4 4 4 1 3 0 1) 
	   (1 1 1 1 1 1 1 1 1)))

;(59)
(setq p10 '((1 1 1 1 1 0 0)
	    (1 4 0 0 1 1 0)
	    (1 3 2 0 0 1 1)
	    (1 1 0 2 0 0 1)
	    (0 1 1 0 2 0 1)
	    (0 0 1 1 0 0 1)
	    (0 0 0 1 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 1 1)))

;(?)
(setq p11 '((0 0 1 0 0 0 0)
	    (0 2 1 4 0 4 0)
	    (0 2 0 4 0 0 0)	   
	    (3 2 1 1 1 4 0)
	    (0 0 1 4 0 0 0)))

;(?)
(setq p12 '((1 1 1 1 1 0 0 0)
	    (1 0 0 4 1 0 0 0)
	    (1 2 1 0 1 1 1 1)
	    (1 4 0 0 0 0 0 1)
	    (1 0 0 5 0 5 0 1)
	    (1 0 5 0 1 0 1 1)
	    (1 1 1 0 3 0 1 0)
	    (0 0 1 1 1 1 1 0)))

;(?)
(setq p13 '((1 1 1 1 1 1 1 1 1 1)
	    (1 3 0 0 1 0 0 4 4 1)
	    (1 0 2 0 2 0 0 4 4 1)
	    (1 0 2 2 2 1 1 4 4 1)
	    (1 0 0 0 0 1 1 4 4 1)
	    (1 1 1 1 1 1 0 0 0 0)))

;(?)
(setq p14 '((0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
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
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)	    
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)
	    ))

;(?)
(setq p15 '((0 0 1 1 1 1 1 1 1 0)
	    (1 1 1 0 0 1 1 1 1 0)
	    (1 0 0 2 0 0 0 1 1 0)
	    (1 3 2 0 2 0 0 0 1 0)
	    (1 1 0 2 0 2 0 0 1 0)
	    (0 1 1 0 2 0 2 0 1 0)
	    (0 0 1 1 0 2 4 0 1 0)
	    (0 0 0 1 1 1 1 0 1 0)
	    (0 0 0 0 1 4 1 0 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 0 1 4 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 1 1 1 1 1)))

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
