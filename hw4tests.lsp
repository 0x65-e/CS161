; Test cases for hw4

(assert (equal (remove-null '(1 2 3 4 5 6) nil) '(6 5 4 3 2 1)))
(assert (equal (remove-null '(1 nil 2 nil 3 4 5 nil 6) nil) '(6 5 4 3 2 1)))
(assert (equal (remove-null nil nil) nil))
(print "All test cases passed for remove-null")

(assert (equal (resolve-or-satisfy '(1 2 3 4 5 6) 4 nil) t)) ; eliminate case
(assert (equal (resolve-or-satisfy '(1 2 3 4 5 6) -4 nil) '(6 5 3 2 1))) ; remove case
(assert (equal (resolve-or-satisfy '(1 2 3 4 5 6) 7 nil) '(6 5 4 3 2 1)))
(assert (equal (resolve-or-satisfy '(1 -2 -3 4 -5 6) -2 nil) t))
(assert (equal (resolve-or-satisfy '(1 2 -3 4 -5 6 2) -2 nil) '(6 -5 4 -3 1)))
(assert (equal (resolve-or-satisfy '(1 2 -3 4 -5 6 -2) -2 nil) t))
(assert (equal (resolve-or-satisfy '(2) -2 nil) nil))
(assert (equal (resolve-or-satisfy nil 1 nil) nil)) ; empty constraint is unsatisfiable
(print "All test cases passed for resolve-or-satisfy")

(assert (equal (resolve-constraints '((1 2) (3 4) (5 6)) 4 nil) '(t ((6 5) (2 1))))) ; satisfied constraint
(assert (equal (resolve-constraints '((1 2) (3 -4) (5 6)) 4 nil) '(t ((6 5) (3) (2 1))))) ; resolved constraint
(assert (equal (resolve-constraints '((1 2) (-4) (5 6)) 4 nil) '(nil nil))) ; unsatisfiable constraint
(assert (equal (resolve-constraints '((4)) 4 nil) '(t nil))) ; satisfied
(print "All test cases passed for resolve-constraints")

(assert (equal (contains-abs '(1 2 3 4 5) 4 nil) 1)) ; positive case
(assert (equal (contains-abs '(1 2 3 4 5) -4 nil) -1)) ; negative case
(assert (equal (contains-abs '(1 2 3 4 5) 7 nil) nil)) ; not found
(assert (equal (contains-abs '(1 2 2 2 3 4 5 6 -1) 1 nil) -1)) ; negative case should take precedence
(assert (equal (contains-abs nil 1 nil) nil))
(print "All test cases passed for contains-abs")

(assert (equal (find-unit '((1 2) (3 4) (5 6))) nil)) ; no unit
(assert (equal (find-unit '((1 2) (3) (5 6))) 3))
(assert (equal (find-unit '((1 2) (3) (5 6) (7))) 3))
(assert (equal (find-unit nil) nil))
(print "All test cases passed for find-unit")

(assert (equal (sat-preempt 1 3 '((1 -2 3) (-1) (-2 -3)) nil) '(3 -2 -1))) ; trivial example
(assert (equal (sat-preempt 1 3 '((1 3) (-1 2) (-2) (-3)) nil) nil)) ; unsatisfiable
(assert (equal (sat-preempt 1 3 '((1 -2) (-1) (-2)) '(-3)) '(-2 -1 -3))) ; force -3
(print "All test cases passed for sat-preempt")

(print "ALL TEST CASES PASSED")

(defun trivial ()
  (solve-cnf "./cnfs/3.cnf")
)

(defun f1 ()
  (time (assert (solve-cnf "./cnfs/f1/sat_f1.cnf")))
)

(defun f2 ()
  (time (assert (solve-cnf "./cnfs/f2/sat_f2.cnf")))
)

(defun f3 ()
  (time (assert (solve-cnf "./cnfs/f3/sat_f3.cnf")))
)

(defun verify (delta assignments)
  (if (null assignments) 
    (null delta)
    (let ((newdelta (resolve-constraints delta (car assignments) nil)))
      (if (car newdelta)
        (verify (cadr newdelta) (cdr assignments))
        nil
      )
    )
  )
)

(defun correct? ()
    (assert (verify (cdr (read-cnf "./cnfs/f1/sat_f1.cnf")) (solve-cnf "./cnfs/f1/sat_f1.cnf")))
    (assert (verify (cdr (read-cnf "./cnfs/f2/sat_f2.cnf")) (solve-cnf "./cnfs/f2/sat_f2.cnf")))
    (assert (verify (cdr (read-cnf "./cnfs/f3/sat_f3.cnf")) (solve-cnf "./cnfs/f3/sat_f3.cnf")))
    "f1-f3 solutions verified"
)

(defun uf20 ()
    (time
        (loop for i from 1 to 1000
        do (assert (solve-cnf (format nil "./cnfs/uf20-91/uf20-0~d.cnf" i)))
        )
    )
)

(defun uf20-correct? ()
    (loop for i from 1 to 1000
        do (assert (verify 
        (remove-null (cdr (read-cnf (format nil "./cnfs/uf20-91/uf20-0~d.cnf" i))) nil) 
        (solve-cnf (format nil "./cnfs/uf20-91/uf20-0~d.cnf" i))))
    )
    "uf20 solutions verified"
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

(defun uf50-correct? ()
    (loop for i from 1 to 1000
        do (assert (verify 
        (remove-null (cdr (read-cnf (format nil "./cnfs/uf50-218/uf50-0~d.cnf" i))) nil) 
        (solve-cnf (format nil "./cnfs/uf50-218/uf50-0~d.cnf" i))))
    )
    "uf50 solutions verified"
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
