; Solutions:
;
; BFS: If TREE is empty, return NIL. If TREE is an atom, return that atom in a
; list. Otherwise, TREE must be a list. If the head is an atom, attach it to
; the result of BFS on the remaining children. If the head is a list, append
; its results to the remaining children (as they should be processed after
; the children at this level - in effect a queue) and recurse.
;
; DFS: Like BFS, checks for an empty TREE or an atom and returns the same. If
; TREE is a list, appends the results of a recursive call on the car with
; the results of a recursive call on the cdr.
;
; DFSMAX: Operates similarly to DFS, but with a DEPTH parameter and in
; right-to-left order. If DEPTH reaches less than zero, returns NIL.
; To accomplish right-to-left ordering, appends the recursive results of the
; cdr to the recursive results of the car. When descending on the car,
; decreases DEPTH by 1.
;
; DFID: Most of the heavy lifting is done by DFMAX. If DEPTH is less than zero,
; returns NIL. Otherwise, appends a recursive call (decreasing DEPTH by 1)
; with a call to DFMAX at the current depth. This is to accomplish the correct
; ordering of depth by 1, 2, ..., DEPTH.

(defun BFS (TREE)
	; Implement breadth-first search on a search tree TREE, which is either
	; an atom or a list of child subtrees. Returns a list of terminal nodes
	; in the order they are visited by a left-to-right BFS.
	(cond
		((null TREE) NIL) ; End of child list
		((atom TREE) (LIST TREE)) ; Leaf node
		((atom (car TREE)) (cons (car TREE) (BFS (cdr TREE))))
		; Head must be is a list
		(T (BFS (append (cdr TREE) (car TREE))))
	)
)

(defun DFS (TREE)
	; Implement depth-first search on a search tree TREE (defined like BFS), 
	; which is either an atom or a list of child subtrees. Returns a list of 
	; terminal nodes in the order they are visited by a left-to-right DFS.
	(cond
		((null TREE) NIL) ; End of child list
		((atom TREE) (LIST TREE)) ; Leaf node
		(T (append (DFS (car TREE)) (DFS (cdr TREE))))
	)
)

(defun DFMAX (TREE DEPTH)
	; This is a helper function for DFID that works the same as DFS (right
	; to left instead of left to right), but will only recursively descend 
	; DEPTH times. If DEPTH reaches less than zero, it returns NIL. Returns a
	; list of terminal not mroe than DEPTH from the root in the order they
	; are visited by a right-to-left DFS.
	(cond
		((< DEPTH 0) NIL)
		((null TREE) NIL) ; End of child list
		((atom TREE) (LIST TREE))
		(T (append (DFMAX (cdr TREE) DEPTH) (DFMAX (car TREE) (- DEPTH 1))))
	)
)

(defun DFID (TREE DEPTH)
	; Takes a tree TREE (like BFS) and the maximum depth of the tree DEPTH. 
	; Returns a list of terminal nodes visited in a right-to-left iterative-
	; deepening search. Nodes visited multiple times will appear multiple times
	; in the output list.
	(cond
		((< DEPTH 0) NIL)
		(T (append (DFID TREE (- DEPTH 1)) (DFMAX TREE DEPTH)))
	)
)

; These functions implement a depth-first solver for the River-Boat
; problem. In this problem, three members from Group-X, denoted XXX,
; and three members from Group-O, denoted OOO, are trying to
; go from the east side of a river to the west side. They have a single boat
; that can carry two people at a time from one side of the river to the
; other. There must be at least one person in the boat to cross the river.
; There can never be more O's on one side of the river than X's.

; In this implementation, a state is represented by a single list
; (#X #O side). side represents which side the boat is
; currently on, and is T if it is on the east side and NIL if on the west
; side. #X and #O represent the number of X's and
; O's on the same side as the boat. Thus, the initial state for this
; problem is (3 3 T) (three X's, three O's, and the boat are all
; on the east side of the river) and the goal state is (3 3 NIL).

; The main entry point for this solver is the function MC-DFS, which is called
; with the initial state to search from and the path to this state. It returns
; the complete path from the initial state to the goal state: this path is a
; list of intermediate problem states. The first element of the path is the
; initial state and the last element is the goal state. Each intermediate state
; is the state that results from applying the appropriate operator to the
; preceding state. If there is no solution, MC-DFS returns NIL.

; To call MC-DFS to solve the original problem, one would call (MC-DFS '(3 3 T)
; NIL) -- however, it would be possible to call MC-DFS with a different initial
; state or with an initial path.



; FINAL-STATE takes a single argument s, the current state, and returns T if it
; is the goal state (3 3 NIL) and NIL otherwise.
(defun final-state (s)
	(equal s '(3 3 NIL))
)

; NEXT-STATE returns the state that results from applying an operator to the
; current state. It takes three arguments: the current state (s), a number of
; X's to move (m), and a number of O's to move (c). It returns a
; list containing the state that results from moving that number of X's
; and O's from the current side of the river to the other side of the
; river. If applying this operator results in an invalid state (because there
; are more O's than X's on either side of the river, or because
; it would move more X's or O's than are on this side of the
; river) it returns NIL.
;
; NOTE that next-state returns a list containing the successor state (which is
; itself a list); the return should look something like ((1 1 T)).
(defun next-state (s m c)
	(let ((newx (- (car s) m))
		(newo (- (cadr s) c))
		(boat (caddr s)))
		(and (>= newx 0) (>= newo 0) (or (= newx 0) (>= newx newo)) (or (= (- 3 newx) 0) (>= (- 3 newx) (- 3 newo))) (list (list (- 3 newx) (- 3 newo) (not boat))))
	)
)

; SUCC-FN returns all of the possible legal successor states to the current
; state. It takes a single argument (s), which encodes the current state, and
; returns a list of each state that can be reached by applying legal operators
; to the current state.
(defun succ-fn (s)
	(append (next-state s 1 0) (next-state s 0 1) (next-state s 1 1) (next-state s 2 0) (next-state s 0 2))
)

; ON-PATH checks whether the current state is on the stack of states visited by
; this depth-first search. It takes two arguments: the current state (s) and the
; stack of states visited by MC-DFS (states). It returns T if s is a member of
; states and NIL otherwise.
(defun on-path (s states)
	(cond
		((null states) NIL)
		((equal s (car states)) T)
		(T (on-path s (cdr states)))
	)
)

; MULT-DFS is a helper function for MC-DFS. It takes two arguments: a stack of
; states from the initial state to the current state (path), and the legal
; successor states from the current state (states).
; MULT-DFS does a depth-first search on each element of states in
; turn. If any of those searches reaches the final state, MULT-DFS returns the
; complete path from the initial state to the goal state. Otherwise, it returns
; NIL. 
; Note that the path should be ordered as: (S_n ... S_2 S_1 S_0)
(defun mult-dfs (states path)
	(cond
		((null states) NIL)
		(T (let ((solution (mc-dfs (car states) path)))
			(cond
				((not (null solution)) solution)
				(T (mult-dfs (cdr states) path))
			)
		))
	)
)

; MC-DFS does a depth first search from a given state to the goal state. It
; takes two arguments: a state (S) and the path from the initial state to S
; (PATH). If S is the initial state in our search, PATH should be NIL. MC-DFS
; performs a depth-first search starting at the given state. It returns the
; path from the initial state to the goal state, if any, or NIL otherwise.
; MC-DFS is responsible for checking if S is already the goal state, as well 
; as for ensuring that the depth-first search does not revisit a node already 
; on the search path.
(defun mc-dfs (s path)
	(cond
		((final-state s) (cons s path))
		((on-path s path) NIL)
		(T (mult-dfs (succ-fn s) (cons s path)))
	)
)

; Test cases for hw2

;(assert (equal (bfs 'A) '(A))) ; Single element case
;(assert (equal (bfs '((A (B)) C (D))) '(C A D B)))
;(assert (equal (bfs '((W X) (Y Z))) '(W X Y Z)))
;(assert (equal (bfs '(A (B C) (D) (E (F G)))) '(A B C D E F G)))
;(assert (equal (bfs '(A ((B) C) (D) (E ((F) G)))) '(A C D E B G F)))
;(print "All test cases passed for BFS")

;(assert (equal (dfs 'A) '(A))) ; Single element case
;(assert (equal (dfs '((A (B)) C (D))) '(A B C D)))
;(assert (equal (dfs '((W X) (Y Z))) '(W X Y Z)))
;(assert (equal (dfs '(A (B C) (D) (E (F G)))) '(A B C D E F G)))
;(print "All test cases passed for DFS")

;(assert (equal (dfid 'A 0) '(A))) ; Single element case
;(assert (equal (dfid '(A) 1) '(A))) ; Single element case
;(assert (equal (dfid '((A (B)) C (D)) 3) '(C D C A D C B A)))
;(assert (equal (dfid '((W X) (Y Z)) 2) '(Z Y X W)))
;(assert (equal (dfid '(A (B C) (D) (E (F G))) 2) '(A E D C B A)))
;(assert (equal (dfid '(A (B C) (D) (E (F G))) 3) '(A E D C B A G F E D C B A)))
;(print "All test cases passed for DFID")

;(assert (equal (final-state '(3 3 T)) NIL))
;(assert (equal (final-state '(3 3 NIL)) T))
;(assert (equal (final-state '(2 3 NIL)) NIL))
;(assert (equal (final-state '(0 1 T)) NIL))
;(print "All test cases passed for final-state")

;(assert (equal (next-state '(3 3 T) 1 0) NIL)) ; Can't have more O's than X's on one side
;(assert (equal (next-state '(2 1 T) 2 0) '((3 2 NIL)))) ; Can have all O's on one side
;(assert (equal (next-state '(1 1 NIL) 2 0) NIL)) ; Can't move more X's than there are
;(assert (equal (next-state '(3 0 NIL) 2 0) NIL)) ; Can't move into an invalid state (more O's on the other side than X's)
;(assert (equal (next-state '(2 2 NIL) 1 1) '((2 2 T)))) ; Valid cases
;(assert (equal (next-state '(3 3 T) 0 1) '((0 1 NIL))))
;(print "All test cases passed for next-state")

;(assert (equal (succ-fn '(3 3 T)) '((0 1 NIL) (1 1 NIL) (0 2 NIL))))
;(assert (equal (succ-fn '(3 1 NIL)) '((0 3 T) (2 2 T))))
;(assert (equal (succ-fn '(2 2 NIL)) '((2 2 T) (3 1 T))))
;(assert (equal (succ-fn '(0 1 T)) '((3 3 NIL))))
;(assert (equal (succ-fn '(3 3 t)) '((0 1 NIL) (1 1 NIL) (0 2 NIL))))
;(assert (equal (succ-fn '(1 1 t)) '((3 2 NIL) (3 3 NIL))))
;(print "All test cases passed for succ-fn")

;(assert (equal (on-path '(1 1 T) NIL) NIL))
;(assert (equal (on-path '(2 1 NIL) '((3 3 T) (2 1 NIL) (0 1 T))) T))
;(assert (equal (on-path '(2 1 NIL) '((3 2 NIL) (2 2 T) (1 0 NIL))) NIL))
;(print "All test cases passed for on-path")

;(assert (equal (mult-dfs NIL NIL) NIL)) ; No successor states
;(assert (equal (mult-dfs '((3 3 NIL)) NIL) '((3 3 NIL)))) ; Final state correct
;(assert (equal (mult-dfs '((1 1 T)) '((3 2 NIL))) '((3 3 NIL) (1 1 T) (3 2 NIL)))) ; Valid solution
;(print "All test cases passed for mult-dfs")

;(assert (equal (mc-dfs '(3 3 T) NIL) '((3 3 NIL) (1 1 T) (3 2 NIL) (0 3 T) (3 1 NIL) (2 2 T) (2 2 NIL) (3 1 T) (0 3 NIL) (3 2 T) (1 1 NIL) (3 3 T)))) ; Just one valid solution, depending on the order of succ-fn
;(assert (equal (mc-dfs '(3 3 NIL) NIL) '((3 3 NIL)))) ; Start at final state
;(assert (equal (mc-dfs '(0 3 NIL) NIL) '((3 3 NIL) (1 1 T) (3 2 NIL) (0 3 T) (3 1 NIL) (2 2 T) (2 2 NIL) (3 1 T) (0 3 NIL))))
;(assert (equal (mc-dfs '(3 0 NIL) NIL) NIL)) ; Impossible state
;(assert (equal (mc-dfs '(0 3 NIL) '((3 2 T) (0 2 NIL) (3 3 T))) '((3 3 NIL) (1 1 T) (3 2 NIL) (0 3 T) (3 1 NIL) (2 2 T) (2 2 NIL) (3 1 T) (0 3 NIL) (3 2 T) (0 2 NIL) (3 3 T))))
;(print "All test cases passed for mc-dfs")

;(print "ALL TEST CASES PASSED")
