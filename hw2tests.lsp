; Test cases for hw2

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


(A (B C) (D) (E (F G)))