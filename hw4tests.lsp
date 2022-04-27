; Test cases for hw4

(assert (equal (remove-null '(1 2 3 4 5 6) nil) '(6 5 4 3 2 1)))
(assert (equal (remove-null '(1 nil 2 nil 3 4 5 nil 6) nil) '(6 5 4 3 2 1)))
(assert (equal (remove-null nil nil) nil))
(print "All test cases passed for remove-null")

(assert (equal (remove-or-eliminate '(1 2 3 4 5 6) 4 nil) '(nil))) ; eliminate case
(assert (equal (remove-or-eliminate '(1 2 3 4 5 6) -4 nil) '((6 5 3 2 1)))) ; remove case
(assert (equal (remove-or-eliminate '(1 2 3 4 5 6) 7 nil) '((6 5 4 3 2 1))))
(assert (equal (remove-or-eliminate '(1 -2 -3 4 -5 6) -2 nil) '(nil)))
(assert (equal (remove-or-eliminate '(1 2 -3 4 -5 6 2) -2 nil) '((6 -5 4 -3 1))))
(assert (equal (remove-or-eliminate '(1 2 -3 4 -5 6 -2) -2 nil) '(nil)))
(assert (equal (remove-or-eliminate '(2) -2 nil) nil))
(assert (equal (remove-or-eliminate nil 1 nil) nil))
(print "All test cases passed for remove-or-eliminate")

(assert (equal (remove-constraints '((1 2) (3 4) (5 6)) 4 nil) '(t ((2 1) (6 5)))))
(assert (equal (remove-constraints '((1 2) (-4) (5 6)) 4 nil) '(nil nil))) ; unsatisfiable
(assert (equal (remove-constraints '((4)) 4 nil) '(t nil))) ; satisfied
(print "All test cases passed for remove-constraints")

(print "ALL TEST CASES PASSED")
