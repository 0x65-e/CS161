; Test cases for hw4

(assert (equal (remove-null '(1 2 3 4 5 6) nil) '(6 5 4 3 2 1)))
(assert (equal (remove-null '(1 nil 2 nil 3 4 5 nil 6) nil) '(6 5 4 3 2 1)))
(assert (equal (remove-null nil nil) nil))
(print "All test cases passed for remove-null")

; TODO: Fix test cases
(assert (equal (resolve-or-satisfy '(1 2 3 4 5 6) 4 nil) t)) ; eliminate case
(assert (equal (resolve-or-satisfy '(1 2 3 4 5 6) -4 nil) '(6 5 3 2 1))) ; remove case
(assert (equal (resolve-or-satisfy '(1 2 3 4 5 6) 7 nil) '(6 5 4 3 2 1)))
(assert (equal (resolve-or-satisfy '(1 -2 -3 4 -5 6) -2 nil) t))
(assert (equal (resolve-or-satisfy '(1 2 -3 4 -5 6 2) -2 nil) '(6 -5 4 -3 1)))
(assert (equal (resolve-or-satisfy '(1 2 -3 4 -5 6 -2) -2 nil) t))
(assert (equal (resolve-or-satisfy '(2) -2 nil) nil))
(assert (equal (resolve-or-satisfy nil 1 nil) nil)) ; empty constraint is unsatisfiable
(print "All test cases passed for resolve-or-satisfy")

(assert (equal (resolve-constraints '((1 2) (3 4) (5 6)) 4 nil) '(t ((6 5) (2 1)))))
(assert (equal (resolve-constraints '((1 2) (-4) (5 6)) 4 nil) '(nil nil))) ; unsatisfiable
(assert (equal (resolve-constraints '((4)) 4 nil) '(t nil))) ; satisfied
(print "All test cases passed for resolve-constraints")

(print "ALL TEST CASES PASSED")


(defun f1 ()
  (time (assert (solve-cnf "./cnfs/f1/sat_f1.cnf")))
)

(defun trivial ()
  (solve-cnf "./cnfs/3.cnf")
)

(defun f2 ()
  (time (assert (solve-cnf "./cnfs/f2/sat_f2.cnf")))
)

(defun f3 ()
  (time (assert (solve-cnf "./cnfs/f3/sat_f3.cnf")))
)

(defun uf20 ()
    (time
    (loop for i from 1 to 1000
      do (assert (solve-cnf (format nil "./cnfs/uf20-91/uf20-0~d.cnf" i)))
    )
    )
)

(defun uf20-i (i)
    (time (print (solve-cnf (format nil "./cnfs/uf20-91/uf20-0~d.cnf" i))))
)

(defun uf20-rand ()
    (let* ((*random-state* (make-random-state t)) (i (random 1000)))
        (print i)
        (time (print (solve-cnf (format nil "./cnfs/uf20-91/uf20-0~d.cnf" i))))
    )
)

(defun uf50 ()
    (loop for i from 1 to 1000
      do (time (assert (solve-cnf (format nil "./cnfs/uf50-218/uf50-0~d.cnf" i))))
    )
)

(defun uf50-i (i)
    (time (solve-cnf (format nil "./cnfs/uf50-218/uf50-0~d.cnf" i)))
)

(defun uf50-rand ()
    (let* ((*random-state* (make-random-state t)) (i (random 1000)))
        (print i)
        (time (solve-cnf (format nil "./cnfs/uf50-218/uf50-0~d.cnf" i)))
    )
)

(defun uuf50 ()
    (loop for i from 1 to 1000
      do (time (assert (not (solve-cnf (format nil "./cnfs/uuf50-218/uuf50-0~d.cnf" i)))))
    )
)

(defun uuf50-i (i)
    (time (assert (not (solve-cnf (format nil "./cnfs/uuf50-218/uuf50-0~d.cnf" i)))))
)

(defun uuf50-rand ()
    (let* ((*random-state* (make-random-state t)) (i (random 1000)))
        (print i)
        (time (assert (not (solve-cnf (format nil "./cnfs/uuf50-218/uuf50-0~d.cnf" i)))))
    )
)
