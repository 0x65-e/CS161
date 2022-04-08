

(defun BFS (TREE)
	(cond
		((null TREE) NIL)
		((atom TREE) (LIST TREE))
		((atom (car TREE)) (cons (car TREE) (BFS (cdr TREE))))
		; Head must be is a list
		(T (BFS (append (cdr TREE) (car TREE))))
	)
)


(defun DFS (TREE)
	(cond
		((null TREE) NIL) ; End of child list
		((atom TREE) (LIST TREE)) ; Leaf node
		(T (append (DFS (car TREE)) (DFS (cdr TREE))))
	)
)

(defun DFMAX (TREE DEPTH)
	(cond
		((< DEPTH 0) NIL)
		((null TREE) NIL) ; End of child list
		((atom TREE) (LIST TREE))
		(T (append (DFMAX (cdr TREE) DEPTH) (DFMAX (car TREE) (- DEPTH 1))))
	)
)

(defun DFID (TREE DEPTH)
	(cond
		((< DEPTH 0) NIL)
		(T (append (DFID TREE (- DEPTH 1)) (DFMAX TREE DEPTH)))
	)
)