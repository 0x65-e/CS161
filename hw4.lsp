;;;;;;;;;;;;;;
; Homework 4 ;
;;;;;;;;;;;;;;

(defun reload ()
  (load "hw4.lsp")
)

(defun test ()
  (load "hw4tests.lsp")
)

; EXERCISE: Modify this function to decide satisifiability of delta.
; If delta is satisfiable, sat? returns a list of n integers that represents a model of delta,  
; otherwise it returns NIL. (See spec for details.)
; param n: number of variables in delta
; param delta: a CNF represented as a list of lists
(defun sat? (n delta)
  (sat-iterative 1 n (remove-null delta nil) nil)
  ;(sat-preempt 1 n (remove-null delta nil) nil)
)

(defun remove-null (lst newl)
  (cond
    ((null lst) newl)
    ((null (car lst)) (remove-null (cdr lst) newl))
    (t (remove-null (cdr lst) (cons (car lst) newl)))
  )
)

; Counting functions

(defun head-append (lst newl)
  (if (null lst) 
    newl
    (head-append (cdr lst) (cons (car lst) newl))
  )
)

(defun flatten (lst newl)
  (if (null lst)
    newl
    (flatten (cdr lst) (head-append (car lst) newl))
  )
)

;
; removet (lst newl)
; removes all instances of t at the top level of lst (i.e. not in a sublist) and returns the new list (reversed)
;
; Helper function of remove-constraints.
;
(defun removet (lst newl)
  (cond
    ((null lst) newl)
    ((eq t (car lst)) (removet (cdr lst) newl))
    (t (removet (cdr lst) (cons (car lst) newl)))
  )
)

; 
; remove-or-eliminate (lst val newl)
; resolves a unit constraint val with lst and returns the resolved constraint.
; If val appears in lst, the resolved constraint will be the symbol t rather than a list.
; If -val appears in lst, it will be removed (but other values will remain). If this results in an empty
; list, the constraint is unsatisfiable and the function will return nil (an empty list).
;
; Helper function of remove-constraints and resolve-constraints.
;
(defun remove-or-eliminate (lst val newl)
  (cond 
    ((null lst) newl)
    ((= (car lst) val) t) ; return t for a satisfied constraint
    ((= (car lst) (- val)) (remove-or-eliminate (cdr lst) val newl)) ; omit this element from newl
    (t (remove-or-eliminate (cdr lst) val (cons (car lst) newl)))
  )
)

; Returns a list of the non-null remaining constraints after resolving the unit constraint val
; Returns (valid newdelta) where valid is nil if there is a contradiction and t otherwise, and newdelta
; is the list of new constraints after resolving.
(defun remove-constraints (delta val newdelta)
  (cond
    ((null delta) (list t newdelta))
    (t (let ((newclause (remove-or-eliminate (car delta) val nil)))
      (cond 
        ((null newclause) (list nil nil))
        ((eq t newclause) (remove-constraints (cdr delta) val newdelta))
        (t (remove-constraints (cdr delta) val (cons newclause newdelta)))
      )
    ))
  )
)

;;;;;;;;;;;;;;;;
; DPLL Search
;;;;;;;;;;;;;;;;

;
; resolve-constraints (delta units newdelta assignments)
; attempts to resolve unit constaints in units with constraints in delta. newdelta contains the results of
; resolution, and assignments contains the variable assignments made (for internal deduplication).
; recurses when new unit constraints are found (additional assignments), until no unit constraints are left.
; returns a list (valid newdelta assignments) where valid is t if there are no contradictions found
; and nil otherwise, newdelta is a new (properly formatted) list of resolved constraints, and assignments
; is a list of values assigned.
;
; Any value assignments present in the parameter assignments must NOT be present in any constraints in delta.
; Neither should units be present in assignments already.
; Inspired by ideas from DPLL search. Helper function of sat-preempt.
;
(defun resolve-constraints (delta units newdelta assignments)
  ;(print units)
  ;(print delta)
  ;(print newdelta)
  (cond
    ((null units) (list t delta assignments)) ; no unit constraints is default a successful resolution
    ((null delta) (resolve-constraints newdelta (cdr units) nil (cons (car units) assignments)))
    ((contains-abs assignments (car units) nil) (list nil nil nil)) ; Conflict found - positive and negative unit clauses
    (t (let* ((newclause (remove-or-eliminate (car delta) (car units) nil)))
      (cond
        ; unsatisfiable constraint (in practice we shouldn't see this, but for safety), quit immediately
        ((null newclause) (list nil nil nil))
        ; satisfied constraint, ignore and move on
        ((eq t newclause) (resolve-constraints (cdr delta) units newdelta assignments))
        ; unit constraint
        ((null (cdr newclause)) (let ((presence (contains-abs units (car newclause) nil)))
          (cond
            ; New unit constraint, add to units (in second position) and move on
            ((null presence) (resolve-constraints (cdr delta) (cons (car units) (cons (car newclause) (cdr units))) newdelta assignments))
            ; Negative constraint, we have a contradition, quit immediately
            ((= -1 presence) (list nil nil nil))
            ; Constraint already in units, ignore and move on
            (t (resolve-constraints (cdr delta) units newdelta assignments))
          )
        ))
        ; non-unit constraint, add to newdelta and move on
        (t (resolve-constraints (cdr delta) units (cons newclause newdelta) assignments))
      )
    ))
  )
)

;
; contains-abs (lst val ret)
; returns -1 if lst contains -val, 1 if lst contains val, and the value of ret otherwise.
; If both +val and -val are present, will return -1.
;
; Helper function for sat-preempt and resolve-constraints.
;
(defun contains-abs (lst val ret)
  (cond
    ((null lst) ret)
    ((= (- (car lst)) val) -1)
    ((= (car lst) val) (contains-abs (cdr lst) val 1))
    (t (contains-abs (cdr lst) val ret))
  )
)

; also requires a version of sat-iterative that checks if start is already in assignments
(defun sat-preempt (start n delta assignments)
  (cond
    ((> start n) assignments)
    ((contains-abs assignments start nil) (sat-preempt (+ start 1) n delta assignments))
    (t (or 
      (let ((pos (resolve-constraints delta (cons start nil) nil nil)))
        (if (car pos)
          (sat-preempt (+ start 1) n (cadr pos) (append (caddr pos) assignments)) ; can't be tail recursive becayse of OR
          nil
        )
      )
      (let ((neg (resolve-constraints delta (cons (- start) nil) nil nil)))
        (if (car neg)
          (sat-preempt (+ start 1) n (cadr neg) (append (caddr neg) assignments))
          nil
        )
      )
    ))
  )
)

;
; find-unit (delta)
; Finds a unit clause in delta (if it exists) and returns its value. If no unit clause exists, returns nil.
;
(defun find-unit (delta)
  (let ((clause (car delta)))
    (cond
      ((null delta) nil)
      ((null (cdr clause)) (car clause))
      (t (find-unit (cdr delta)))
    )
  )
)

;
; sat-iterative (start n delta assignments)
; Basic backtracking solver. Starts with variable start and proceeds iteratively until n. 
; Values are added to assignments as they are checked.
;
(defun sat-iterative (start n delta assignments)
  (cond 
    ((> start n) assignments)
    ((contains-abs assignments start nil) (sat-iterative (+ start 1) n delta assignments))
    (t (let ((unit (find-unit delta)))
      ; If there's a unit constraint, check that first
      (if unit
        (let ((res (remove-constraints delta unit nil)))
          (if (car res)
            (sat-iterative start n (cadr res) (cons unit assignments))
            nil
          )
        )
        ; Otherwise default to the next unassigned number
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
    ))
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

