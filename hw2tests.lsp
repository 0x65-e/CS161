; Test cases for hw2

; TODO: Write more test cases

(assert (equal (bfs '((A (B)) C (D))) '(C A D B)))
(assert (equal (bfs '(A (B C) (D) (E (F G)))) '(A B C D E F G)))
(print "All test cases passed for BFS")

(assert (equal (dfs '((A (B)) C (D))) '(A B C D)))
(assert (equal (dfs '((W X) (Y Z))) '(W X Y Z)))
(assert (equal (dfs '(A (B C) (D) (E (F G)))) '(A B C D E F G)))
(print "All test cases passed for DFS")

(assert (equal (dfid '((A (B)) C (D)) 3) '(C D C A D C B A)))
(assert (equal (dfid '(A (B C) (D) (E (F G))) 2) '(A E D C B A)))
(print "All test cases passed for DFID")

(assert (equal (final-state '(3 3 T)) NIL))
(assert (equal (final-state '(3 3 NIL)) T))
(assert (equal (final-state '(2 3 NIL)) NIL))
(assert (equal (final-state '(0 1 T)) NIL))
(print "All test cases passed for final-state")

(assert (equal (next-state '(3 3 T) 1 0) NIL)) ; Can't have more O's than X's on one side
(assert (equal (next-state '(2 1 T) 2 0) '((3 2 NIL)))) ; Can have all O's on one side
(assert (equal (next-state '(1 1 NIL) 2 0) NIL)) ; Can't move more X's than there are
(assert (equal (next-state '(3 0 NIL) 2 0) NIL)) ; Can't move into an invalid state (more O's on the other side than X's)
(assert (equal (next-state '(2 2 NIL) 1 1) '((2 2 T)))) ; Valid cases
(assert (equal (next-state '(3 3 T) 0 1) '((0 1 NIL))))
(print "All test cases passed for next-state")

(assert (equal (succ-fn '(3 3 T)) '((0 1 NIL) (1 1 NIL) (0 2 NIL))))
(assert (equal (succ-fn '(3 1 NIL)) '((0 3 T) (2 2 T))))
(assert (equal (succ-fn '(2 2 NIL)) '((2 2 T) (3 1 T))))
(assert (equal (succ-fn '(0 1 T)) '((3 3 NIL))))
(assert (equal (succ-fn '(3 3 t)) '((0 1 NIL) (1 1 NIL) (0 2 NIL))))
(assert (equal (succ-fn '(1 1 t)) '((3 2 NIL) (3 3 NIL))))
(print "All test cases passed for succ-fn")

(assert (equal (on-path '(2 1 NIL) '((3 3 T) (2 1 NIL) (0 1 T))) T))
(assert (equal (on-path '(2 1 NIL) '((3 2 NIL) (2 2 T) (1 0 NIL))) NIL))
(print "All test cases passed for on-path")

; Add test cases for mult-dfs

; Add more test cases for mc-dfs
(assert (equal (mc-dfs '(3 3 T) NIL) '((3 3 NIL) (1 1 T) (3 2 NIL) (0 3 T) (3 1 NIL) (2 2 T) (2 2 NIL) (3 1 T) (0 3 NIL) (3 2 T) (1 1 NIL) (3 3 T)))) ; Just one valid solution, depending on the order of succ-fn
(assert (equal (mc-dfs '(3 3 NIL) NIL) '((3 3 NIL)))) ; Start at final state
(assert (equal (mc-dfs '(0 3 NIL) NIL) '((3 3 NIL) (1 1 T) (3 2 NIL) (0 3 T) (3 1 NIL) (2 2 T) (2 2 NIL) (3 1 T) (0 3 NIL))))
(assert (equal (mc-dfs '(3 0 NIL) NIL) NIL)) ; Impossible state
(assert (equal (mc-dfs '(0 3 NIL) '((3 2 T) (0 2 NIL) (3 3 T))) '((3 3 NIL) (1 1 T) (3 2 NIL) (0 3 T) (3 1 NIL) (2 2 T) (2 2 NIL) (3 1 T) (0 3 NIL) (3 2 T) (0 2 NIL) (3 3 T))))
(print "All test cases passed for mc-dfs")