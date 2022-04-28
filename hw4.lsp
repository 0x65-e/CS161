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
  (sat-preempt 1 n (remove-null delta nil) nil)
)

; Helper function to remove null values from a list (for testing uuf50/uf50)
(defun remove-null (lst newl)
  (cond
    ((null lst) newl)
    ((null (car lst)) (remove-null (cdr lst) newl))
    (t (remove-null (cdr lst) (cons (car lst) newl)))
  )
)

; 
; resolve-or-satisfy (lst val newl)
; resolves a unit constraint val with lst and returns the resolved constraint.
; If val appears in lst, the constraint will be satisfied as the symbol t rather than a list.
; If -val appears in lst, it will be removed (but other values will remain). If this results in an empty
; list, the constraint is unsatisfiable and the function will return nil (an empty list).
;
; Helper function of resolve-constraints.
;
(defun resolve-or-satisfy (lst val newl)
  (cond 
    ((null lst) newl)
    ((= (car lst) val) t) ; return t for a satisfied constraint
    ((= (car lst) (- val)) (resolve-or-satisfy (cdr lst) val newl)) ; omit this element from newl
    (t (resolve-or-satisfy (cdr lst) val (cons (car lst) newl)))
  )
)

;
; resolve-constraints (delta unit newdelta)
; attempts to resolve the unit constaint unit with constraints in delta. newdelta contains the results of resolution.
; returns a list (valid newdelta) where valid is nil if a contradiction is found and t otherwise,
; and newdelta is a new (properly formatted) list of constraints after resolution.
;
; Helper function of sat-preempt.
;
(defun resolve-constraints (delta unit newdelta)
  (cond
    ((null delta) (list t newdelta))
    (t (let ((newclause (resolve-or-satisfy (car delta) unit nil)))
      (cond
        ((null newclause) (list nil nil))
        ((eq t newclause) (resolve-constraints (cdr delta) unit newdelta))
        (t (resolve-constraints (cdr delta) unit (cons newclause newdelta)))
      )
    ))
  )
)

;
; contains-abs (lst val ret)
; returns -1 if lst contains -val, 1 if lst contains val, and the value of ret otherwise.
; If both +val and -val are present, will return -1.
;
; Helper function for sat-preempt.
;
(defun contains-abs (lst val ret)
  (cond
    ((null lst) ret)
    ((= (- (car lst)) val) -1)
    ((= (car lst) val) (contains-abs (cdr lst) val 1))
    (t (contains-abs (cdr lst) val ret))
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
; sat-preepmt (start n delta assignments)
; Backtracking SAT solver. The first unsatisfied variable is start, and the last is n. delta contains
; the constraints. assignments is a list of required variable assignments (for internal use).
;
; Inspired by ideas from DPLL search (i.e. resolve all unit constraints first). If there is a unit constraint
; in delta, first resolve that variable (hence, it "preempts" branching on start) and then recurse with the
; same start. Otherwise, check if start has already been resolved (i.e. if it is present in assignments). 
; If so, move on and advance start by 1. If not, try resolving start, either positive or negative (a branch). 
; If at any point an unsatisfiable constraint or contradiction is found, backtrack (by returning nil).
;
(defun sat-preempt (start n delta assignments)
  (cond
    ((> start n) assignments) ; All variables are assigned and no contradictions were found
    ; Check if start has already been assigned a value
    ((contains-abs assignments start nil) (sat-preempt (+ start 1) n delta assignments))
    (t (let ((unit (find-unit delta)))
      ; If there's a unit constraint in delta, check that first and then recurse with the same start
      (if unit
        (let ((res (resolve-constraints delta unit nil)))
          (if (car res)
            (sat-preempt start n (cadr res) (cons unit assignments))
            nil
          )
        )
        ; Otherwise default to the next unassigned number and branch
        (or
          (let ((pos (resolve-constraints delta start nil)))
            (if (car pos)
              (sat-preempt (+ start 1) n (cadr pos) (cons start assignments))
              nil
            )
          )
          (let ((neg (resolve-constraints delta (- start) nil)))
            (if (car neg)
              (sat-preempt (+ start 1) n (cadr neg) (cons (- start) assignments))
              nil
            )
          )
        )
      )
    ))
  )
)


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

