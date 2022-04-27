;;;;;;;;;;;;;;
; Homework 4 ;
;;;;;;;;;;;;;;

(defun reload ()
  (load "hw4.lsp")
)

(defun test ()
  (load "hw4tests.lsp")
)

(defun f1 ()
  (solve-cnf "./cnfs/f1/sat_f1.cnf")
)

(defun f2 ()
  (solve-cnf "./cnfs/f2/sat_f2.cnf")
)

(defun f3 ()
  (solve-cnf "./cnfs/f3/sat_f3.cnf")
)

; EXERCISE: Modify this function to decide satisifiability of delta.
; If delta is satisfiable, sat? returns a list of n integers that represents a model of delta,  
; otherwise it returns NIL. (See spec for details.)
; param n: number of variables in delta
; param delta: a CNF represented as a list of lists
(defun sat? (n delta)
  (sat-iterative 1 n delta nil)
)

(defun remove-null (lst newl)
  (cond
    ((null lst) newl)
    ((null (car lst)) (remove-null (cdr lst) newl))
    (t (remove-null (cdr lst) (cons (car lst) newl)))
  )
)

(defun remove-or-eliminate (lst val newl)
  (cond 
    ((null lst) (if (null newl) nil (cons newl nil))) ; return nil only if newl was empty, otherwise a list containing newl
    ((= (car lst) val) (cons nil nil))
    ((= (car lst) (- val)) (remove-or-eliminate (cdr lst) val newl)) ; omit this element from newl
    (t (remove-or-eliminate (cdr lst) val (cons (car lst) newl)))
  )
)

; Returns a list of the non-null remaining constraints after selecting val
; If the result is a number, means there is no solution for at least one constraint
(defun remove-constraints (delta val newdelta)
  (cond
    ((null delta) (list t (remove-null newdelta nil)))
    (t (let ((newclause (remove-or-eliminate (car delta) val nil)))
      (if (null newclause)
        (list nil nil)
        (remove-constraints (cdr delta) val (cons (car newclause) newdelta))
      )
    ))
  )
)

(defun sat-iterative (start n delta assignments)
  (if (> start n) 
    assignments
    (or 
      (let ((pos (remove-constraints delta start nil)))
        (if (car pos)
          (sat-iterative (+ start 1) n (cadr pos) (cons start assignments))
          nil
        )
      )
      (let ((neg (remove-constraints delta (- start) nil)))
        (if (car neg)
          (sat-iterative (+ start 1) n (cadr neg) (cons (- start) assignments))
          nil
        )
      )
    )
  )
)

; Simple solution: Start with 1 ... n, try both positive and negative values
; Check constraints if they could be satisfied by remaining assignments
; Remove from constraints - if present, remove constraint. If not present, remove *from* constraint and add back



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Functions that help you parse CNF from files in folder cnfs/
; You need not modify any functions in this section
; Usage (solve-cnf <path-to-file>)
; e.g., (solve-cnf "./cnfs/f1/sat_f1.cnf")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun split-line (line)
  (if (equal line :eof)
      :eof
      (with-input-from-string (s line) (loop for x = (read s nil) while x collect x))))

(defun read-cnf (filename)
  (with-open-file (in filename)
    (loop for line = (split-line (read-line in nil :eof)) until (equal line :eof)
      if (equal 'p (first line)) collect (third line)      ; var count
      if (integerp (first line)) collect (butlast line)))) ; clause

(defun parse-cnf (filename)
  (let ((cnf (read-cnf filename))) (list (car cnf) (cdr cnf))))

; Following is a helper function that combines parse-cnf and sat?
(defun solve-cnf (filename)
  (let ((cnf (parse-cnf filename))) (sat? (first cnf) (second cnf))))

