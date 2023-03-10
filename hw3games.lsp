; games

(defun h0-time ()
	(progn
	(print "h0-time")
	(print "p1")
	(time (a* p1 #'goal-test #'next-states #'h0))
	(print "")
	(print "p2")
	(time (a* p2 #'goal-test #'next-states #'h0))
	(print "")
	(print "p3")
	(time (a* p3 #'goal-test #'next-states #'h0))
	(print "")
	(print "p4")
	(time (a* p4 #'goal-test #'next-states #'h0))
	(print "")
	(print "p5")
	(time (a* p5 #'goal-test #'next-states #'h0))
	(print "")
	(print "p6")
	(time (a* p6 #'goal-test #'next-states #'h0))
	(print "")
	(print "p7")
	(time (a* p7 #'goal-test #'next-states #'h0))
	(print "")
	(print "p8")
	(time (a* p8 #'goal-test #'next-states #'h0))
	(print "")
	(print "p9")
	(time (a* p9 #'goal-test #'next-states #'h0))
	(print "")
	(print "p10")
	(time (a* p10 #'goal-test #'next-states #'h0))
	(print "")
	)
)

(defun h0-hard-time ()
	(progn
	(print "h0-hard-time")
	(print "p11")
	(time (a* p11 #'goal-test #'next-states #'h0))
	(print "")
	(print "p12")
	(time (a* p12 #'goal-test #'next-states #'h0))
	(print "")
	(print "p13")
	(time (a* p13 #'goal-test #'next-states #'h0))
	(print "")
	(print "p14")
	(time (a* p14 #'goal-test #'next-states #'h0))
	(print "")
	(print "p15")
	(time (a* p15 #'goal-test #'next-states #'h0))
	(print "")
	)
)

(defun h1-games (delay)
	(printstates (a* p1 #'goal-test #'next-states #'h1) delay)
	(printstates (a* p2 #'goal-test #'next-states #'h1) delay)
	(printstates (a* p3 #'goal-test #'next-states #'h1) delay)
	(printstates (a* p4 #'goal-test #'next-states #'h1) delay)
	(printstates (a* p5 #'goal-test #'next-states #'h1) delay)
	(printstates (a* p6 #'goal-test #'next-states #'h1) delay)
	(printstates (a* p7 #'goal-test #'next-states #'h1) delay)
	(printstates (a* p8 #'goal-test #'next-states #'h1) delay)
	(printstates (a* p9 #'goal-test #'next-states #'h1) delay)
	(printstates (a* p10 #'goal-test #'next-states #'h1) delay)
)

(defun h1-results ()
	(progn
	(a* p1 #'goal-test #'next-states #'h1)
	(a* p2 #'goal-test #'next-states #'h1)
	(a* p3 #'goal-test #'next-states #'h1)
	(a* p4 #'goal-test #'next-states #'h1)
	(a* p5 #'goal-test #'next-states #'h1)
	(a* p6 #'goal-test #'next-states #'h1)
	(a* p7 #'goal-test #'next-states #'h1)
	(a* p8 #'goal-test #'next-states #'h1)
	(a* p9 #'goal-test #'next-states #'h1)
	(a* p10 #'goal-test #'next-states #'h1)
	(values)
	)
)

(defun h1-time ()
	(progn
	(print "h1-time")
	(print "p1")
	(time (a* p1 #'goal-test #'next-states #'h1))
	(print "")
	(print "p2")
	(time (a* p2 #'goal-test #'next-states #'h1))
	(print "")
	(print "p3")
	(time (a* p3 #'goal-test #'next-states #'h1))
	(print "")
	(print "p4")
	(time (a* p4 #'goal-test #'next-states #'h1))
	(print "")
	(print "p5")
	(time (a* p5 #'goal-test #'next-states #'h1))
	(print "")
	(print "p6")
	(time (a* p6 #'goal-test #'next-states #'h1))
	(print "")
	(print "p7")
	(time (a* p7 #'goal-test #'next-states #'h1))
	(print "")
	(print "p8")
	(time (a* p8 #'goal-test #'next-states #'h1))
	(print "")
	(print "p9")
	(time (a* p9 #'goal-test #'next-states #'h1))
	(print "")
	(print "p10")
	(time (a* p10 #'goal-test #'next-states #'h1))
	(print "")
	)
)

(defun h1-hard-time ()
	(progn
	(print "h1-hard-time")
	(print "p11")
	(time (a* p11 #'goal-test #'next-states #'h1))
	(print "")
	(print "p12")
	(time (a* p12 #'goal-test #'next-states #'h1))
	(print "")
	(print "p13")
	(time (a* p13 #'goal-test #'next-states #'h1))
	(print "")
	(print "p14")
	(time (a* p14 #'goal-test #'next-states #'h1))
	(print "")
	(print "p15")
	(time (a* p15 #'goal-test #'next-states #'h1))
	(print "")
	)
)

(defun h1-total-time ()
	(print "h1-total-time")
	(time (progn
		(print "p1")
		(a* p1 #'goal-test #'next-states #'h1)
		(print "")
		(print "p2")
		(a* p2 #'goal-test #'next-states #'h1)
		(print "")
		(print "p3")
		(a* p3 #'goal-test #'next-states #'h1)
		(print "")
		(print "p4")
		(a* p4 #'goal-test #'next-states #'h1)
		(print "")
		(print "p5")
		(a* p5 #'goal-test #'next-states #'h1)
		(print "")
		(print "p6")
		(a* p6 #'goal-test #'next-states #'h1)
		(print "")
		(print "p7")
		(a* p7 #'goal-test #'next-states #'h1)
		(print "")
		(print "p8")
		(a* p8 #'goal-test #'next-states #'h1)
		(print "")
		(print "p9")
		(a* p9 #'goal-test #'next-states #'h1)
		(print "")
		(print "p10")
		(a* p10 #'goal-test #'next-states #'h1)
		(print "")
		(print "p11")
		(a* p11 #'goal-test #'next-states #'h1)
		(print "")
		(print "p12")
		(a* p12 #'goal-test #'next-states #'h1)
		(print "")
		)
	)
)

(defun h2-time ()
	(progn
	(print "h2-time")
	(print "p1")
	(time (a* p1 #'goal-test #'next-states #'h2))
	(print "")
	(print "p2")
	(time (a* p2 #'goal-test #'next-states #'h2))
	(print "")
	(print "p3")
	(time (a* p3 #'goal-test #'next-states #'h2))
	(print "")
	(print "p4")
	(time (a* p4 #'goal-test #'next-states #'h2))
	(print "")
	(print "p5")
	(time (a* p5 #'goal-test #'next-states #'h2))
	(print "")
	(print "p6")
	(time (a* p6 #'goal-test #'next-states #'h2))
	(print "")
	(print "p7")
	(time (a* p7 #'goal-test #'next-states #'h2))
	(print "")
	(print "p8")
	(time (a* p8 #'goal-test #'next-states #'h2))
	(print "")
	(print "p9")
	(time (a* p9 #'goal-test #'next-states #'h2))
	(print "")
	(print "p10")
	(time (a* p10 #'goal-test #'next-states #'h2))
	(print "")
	)
)

(defun h2-hard-time ()
	(progn
	(print "h2-hard-time")
	(print "p11")
	(time (a* p11 #'goal-test #'next-states #'h2))
	(print "")
	(print "p12")
	(time (a* p12 #'goal-test #'next-states #'h2))
	(print "")
	(print "p13")
	(time (a* p13 #'goal-test #'next-states #'h2))
	(print "")
	(print "p14")
	(time (a* p14 #'goal-test #'next-states #'h2))
	(print "")
	(print "p15")
	(time (a* p15 #'goal-test #'next-states #'h2))
	(print "")
	)
)

(defun h2-total-time ()
	(print "h2-total-time")
	(time (progn
		(print "p1")
		(a* p1 #'goal-test #'next-states #'h2)
		(print "")
		(print "p2")
		(a* p2 #'goal-test #'next-states #'h2)
		(print "")
		(print "p3")
		(a* p3 #'goal-test #'next-states #'h2)
		(print "")
		(print "p4")
		(a* p4 #'goal-test #'next-states #'h2)
		(print "")
		(print "p5")
		(a* p5 #'goal-test #'next-states #'h2)
		(print "")
		(print "p6")
		(a* p6 #'goal-test #'next-states #'h2)
		(print "")
		(print "p7")
		(a* p7 #'goal-test #'next-states #'h2)
		(print "")
		(print "p8")
		(a* p8 #'goal-test #'next-states #'h2)
		(print "")
		(print "p9")
		(a* p9 #'goal-test #'next-states #'h2)
		(print "")
		(print "p10")
		(a* p10 #'goal-test #'next-states #'h2)
		(print "")
		(print "p11")
		(a* p11 #'goal-test #'next-states #'h2)
		(print "")
		(print "p12")
		(a* p12 #'goal-test #'next-states #'h2)
		(print "")
		)
	)
)
