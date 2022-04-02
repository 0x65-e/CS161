; Provided test cases for hw1

(assert (eq (TREE-CONTAINS 3 '((1 2 3) 7 8)) 'T)) ; T case
(assert (eq (TREE-CONTAINS 4 '((1 2 3) 7 8)) 'NIL)) ; NIL case
(assert (eq (TREE-CONTAINS 12 '((4 6 7) 9 (10 11 12))) 'T)) ; Descend right
(assert (eq (TREE-CONTAINS 3 3) 'T)) ; Number case
(print "All test cases passed for TREE-CONTAINS")

(assert (eq (TREE-MAX '((1 2 3) 7 8)) 8))
(assert (eq (TREE-MAX '(1 2 (3 7 8))) 8)) ; Descends down right subtree
(assert (eq (TREE-MAX 4) 4)) ; Number case
(print "All test cases passed for TREE-MAX")

(assert (equal (TREE-ORDER 3) '(3))) ; Single number
(assert (equal (TREE-ORDER '((1 2 3) 7 8)) '(1 3 2 8 7))) ; Valid tree
(assert (equal (TREE-ORDER '((1 2 3) 7 (8 9 10))) '(1 3 2 8 10 9 7)))
(print "All test cases passed for TREE-ORDER")

(assert (equal (SUB-LIST '(a b c d) 0 3) '(a b c)))
(assert (equal (SUB-LIST '(a b c d) 3 1) '(d))) ; Non-zero start
(assert (equal (SUB-LIST '(a b c d) 2 0) 'NIL)) ; Zero length
(assert (equal (SUB-LIST '(a b c d) 15 10) 'NIL)) ; Start past end of array
(assert (equal (SUB-LIST '(a b c d) 2 4) '(c d))) ; Length past end of array
(assert (equal (SUB-LIST '(a 1 b 2) 1 2) '(1 b))) ; Mixed atoms
(assert (equal (SUB-LIST '(a NIL b 3 2 NIL NIL 7) 2 4) '(b 3 2 NIL))) ; NIL atoms
(print "All test cases passed for SUB-LIST")

(assert (equal (SPLIT-LIST '(a b c d)) '((a b) (c d)))) ; Even length
(assert (equal (SPLIT-LIST '(a b c d e)) '((a b c) (d e)))) ; Odd length
(assert (equal (SPLIT-LIST '(a b c d e f)) '((a b c) (d e f))))
(assert (equal (SPLIT-LIST '(1 2 3 4 5 6)) '((1 2 3) (4 5 6)))) ; Numbers
(print "All test cases passed for SPLIT-LIST")

(assert (eq (BTREE-HEIGHT 1) 0)) ; Trivial root
(assert (eq (BTREE-HEIGHT '(1 2)) 1)) ; Single level
(assert (eq (BTREE-HEIGHT '(1 (2 3))) 2)) ; Multi-level tree
(assert (eq (BTREE-HEIGHT '((1 2) (3 4))) 2))
(assert (eq (BTREE-HEIGHT '((1 (2 3)) ((4 5) (6 7)))) 3))
(assert (eq (BTREE-HEIGHT '((a (b c)) ((d e) (f g)))) 3)) ; Non-numeric atoms
(assert (eq (BTREE-HEIGHT '(((1 2) (3 4)) ((5 6) (7 8)))) 3))
(print "All test cases passed for BTREE-HEIGHT")

(assert (equal (LIST2BTREE '(1)) 1)) ; Single number
(assert (equal (LIST2BTREE '(a)) 'a)) ; Non-numeric atom
(assert (equal (LIST2BTREE '(1 2)) '(1 2))) ; Already valid 
(assert (equal (LIST2BTREE '(1 2 3)) '((1 2) 3))) ; Split left on odd
(assert (equal (LIST2BTREE '(1 2 3 4)) '((1 2) (3 4)))) ; Even split on even
(assert (equal (LIST2BTREE '(1 2 3 4 5 6 7)) '(((1 2) (3 4)) ((5 6) 7))))
(assert (equal (LIST2BTREE '(1 2 3 4 5 6 7 8)) '(((1 2) (3 4)) ((5 6) (7 8))))) ; Multi-level split
(assert (equal (LIST2BTREE '(1 a 2 b 3 c)) '(((1 a) 2) ((b 3) c)))) ; Multiple non-numeric atoms
(print "All test cases passed for LIST2BTREE")

(assert (equal (BTREE2LIST 1) '(1)))
(assert (equal (BTREE2LIST '(1 2)) '(1 2)))
(assert (equal (BTREE2LIST '((1 2) 3)) '(1 2 3)))
(assert (equal (BTREE2LIST '((1 2) (3 4))) '(1 2 3 4)))
(assert (equal (BTREE2LIST '(((1 2) (3 4)) ((5 6) 7))) '(1 2 3 4 5 6 7)))
(assert (equal (BTREE2LIST '(((1 2) (3 4)) ((5 6) (7 8)))) '(1 2 3 4 5 6 7 8)))
(assert (equal (BTREE2LIST '((a (3 NIL)) (((c d) 6) (7 (e g))))) '(a 3 NIL c d 6 7 e g))) ; Non-numeric atoms, including NIL
(print "All test cases passed for BTREE2LIST")

(assert (eq (IS-SAME '((1 2 3) 7 8) '((1 2 3) 7 8)) 'T)) ; T case
(assert (eq (IS-SAME '(1 2 3 7 8) '((1 2 3) 7 8)) 'NIL)) ; NIL case
(assert (eq (IS-SAME 3 3) 'T)) ; Atom (number) T case
(assert (eq (IS-SAME 3 4) 'NIL)) ; Atom (number) NIL case
(assert (eq (IS-SAME 1 '(2 3)) 'NIL)) ; Mismatched number and list
(print "All test cases passed for IS-SAME")

(assert (equal (FLATTEN-APPEND '(0 1) 'NIL) '(0 1))) ; NIL E2 case
(assert (equal (FLATTEN-APPEND '(0 1) 2) '(0 1 2))) ; Atom E2 case
(assert (equal (FLATTEN-APPEND '(0 1) '(2 (3 4) 5 6)) '(0 1 2 3 4 5 6))) ; Flatten E2
(assert (equal (FLATTEN-APPEND '(0 1) '(2 ((((3))) 4) 5 6)) '(0 1 2 3 4 5 6))) ; Flatten E2 with multiple deep single list
(assert (equal (FLATTEN-APPEND '(0 (1 (2 3)) 4)  '(5 (6) 7)) '(0 (1 (2 3)) 4 5 6 7))) ; Don't flatten E1
(assert (equal (FLATTEN-APPEND 'NIL '(1 2 ((3) 4) 5 (6 7) 8)) '(1 2 3 4 5 6 7 8))) ; Multi-level list in E2
(print "All test cases passed for FLATTEN-APPEND")

(print "ALL TEST CASES PASSED")