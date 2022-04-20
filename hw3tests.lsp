; Test cases for hw3

(setq blank 0)
(setq wall 1)
(setq box 2)
(setq keeper 3)
(setq star 4)
(setq boxstar 5)
(setq keeperstar 6)

(assert (equal (cleanUpList '(0 1 2 3 4 5)) '(0 1 2 3 4 5)))
(assert (equal (cleanUpList '()) '()))
(assert (equal (cleanUpList '(0 1 nil 2 3 nil 4 5)) '(0 1 2 3 4 5)))
(print "All test cases passed for cleanUpList")

(assert (equal (countListContents '(0 1 1 0 0 0 1 2 1) 2 0) 1))
(assert (equal (countListContents '() 0 2) 2))
(assert (equal (countListContents '(0 1 1 0 0 0 1 2 1) 1 1) 5))
(print "All test cases passed for countListContents")

(assert (equal (countStateContents '((1 2) (3 4) (5 6)) 3 0) 1))
(assert (equal (countStateContents '((0 0 0) (1 1 1) (2 2 2)) 2 2) 5))
(assert (equal (countStateContents '() 2 3) 3))
(print "All test cases passed for countStateContents")

(assert (equal (goal-test (list (list box blank) (list blank keeperstar))) nil))
(assert (equal (goal-test (list (list blank blank) (list star keeper))) nil))
(assert (equal (goal-test (list (list blank blank) (list wall keeperstar))) T))
(assert (equal (goal-test (list (list boxstar boxstar) (list keeperstar wall))) T))
(print "All test cases passed for goal-state")

(assert (equal (getColumnSeq '() 1 1 3 '(1)) '(nil nil nil 1)))
(assert (equal (getColumnSeq '((1 2 3) (4 5 6) (7 8 9)) 1 1 2 '()) '(8 5)))
(assert (equal (getColumnSeq '((1 2 3) (4 5 6) (7 8 9)) 1 -1 2 '()) '(2 nil)))
(assert (equal (getColumnSeq '((1 2 3) (4 5 6) (7 8 9)) 1 1 3 '()) '(nil 8 5)))
(print "All test cases passed for getColumnSeq")

(assert (equal (getSubSeq '() 4 3 '(1)) '(nil nil nil 1)))
(assert (equal (getSubSeq '(1 2 3 4 5 6 7 8 9) 1 2 '()) '(3 2)))
(assert (equal (getSubSeq '(1 2 3 4 5 6 7 8 9) -1 2 '()) '(1 nil)))
(assert (equal (getSubSeq '(1 2 3 4 5 6 7 8 9) 6 4 '()) '(nil 9 8 7)))
(print "All test cases passed for getSubSeq")

(assert (equal (getRowSeq '() 1 1 3 '(2)) '(nil nil nil 2)))
(assert (equal (getRowSeq '((1 2 3) (4 5 6) (7 8 9)) 1 1 2 '()) '(6 5)))
(assert (equal (getRowSeq '((1 2 3) (4 5 6) (7 8 9)) 1 -1 2 '()) '(4 nil)))
(assert (equal (getRowSeq '((1 2 3) (4 5 6) (7 8 9)) 1 1 3 '()) '(nil 6 5)))
(print "All test cases passed for getRowSeq")

(assert (equal (replaceSubSeq '() 3 '(1 2 3)) '()))
(assert (equal (replaceSubSeq '(1 2 3 4 5 6) 1 '()) '(1 2 3 4 5 6)))
(assert (equal (replaceSubSeq '(1 2 3 4 5 6) 2 '(8 9)) '(1 2 8 9 5 6)))
(assert (equal (replaceSubSeq '(1 2 3 4 5 6) -1 '(22 33 44)) '(33 44 3 4 5 6)))
(assert (equal (replaceSubSeq '(1 2 3 4 5 6) 4 '(77 88 99)) '(1 2 3 4 77 88)))
(print "All test cases passed for replaceSubSeq")

(assert (equal (replaceColSeq '() 4 3 '(1 2)) '()))
(assert (equal (replaceColSeq '((1 2 3) (4 5 6) (7 8 9)) 1 1 '(12 13)) '((1 2 3) (4 12 6) (7 13 9))))
(assert (equal (replaceColSeq '((1 2 3) (4 5 6) (7 8 9)) 1 -1 '(12 13 14 15 16 17)) '((1 13 3) (4 14 6) (7 15 9))))
(assert (equal (replaceColSeq '((1 2 3) (4 5 6) (7 8 9)) 1 1 '()) '((1 2 3) (4 5 6) (7 8 9))))
(print "All test cases passed for replaceColSeq")

(assert (equal (replaceRowSeq '() 2 3 '(2 3 4)) '()))
(assert (equal (replaceRowSeq '((1 2 3) (4 5 6) (7 8 9)) 1 1 '(12 13)) '((1 2 3) (4 12 13) (7 8 9))))
(assert (equal (replaceRowSeq '((1 2 3) (4 5 6) (7 8 9)) 1 -1 '(21 22 23 24 25)) '((1 2 3) (22 23 24) (7 8 9))))
(assert (equal (replaceRowSeq '((1 2 3) (4 5 6) (7 8 9)) 1 1 '()) '((1 2 3) (4 5 6) (7 8 9))))
(print "All test cases passed for replaceRowSeq")

(print "All test cases passed")