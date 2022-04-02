; Solutions:
; 
; TREE-CONTAINS: If TREE is a leaf, check if the leaf is N.
; Otherwise, TREE must be an internal node, so check if N
; is greater or less than the split number and recurse on that subtree.
;
; TREE-MAX: If TREE is a leaf, that is the max by default. Otherwise,
; the max must be in the right subtree, which is a  valid ordered tree 
; by definition.
;
; TREE-ORDER: If TREE is a leaf, return a list of that number. Otherwise,
; recurse on the left and right subtrees, and then append those with a list
; of the split number.
;
; SUB-LIST: Recurse on the cdr of the list until we hit the first element 
; of the sublist (the START element). Then cons that element onto the head
; of the (LEN-1) sublist from the cdr of the list. Stop when the length
; runs out (or if we encounter the end of the list early).
;
; SPLIT-LIST: Determine the length of the first and second lists. If L is
; even length, both will be the length divided by 2. Otherwise, the first
; list will be one longer than the second list. Then use SUB-LIST to divide
; the original list using the lengths.
;
; BTREE-HEIGHT: The base case is a single leaf node, with height 0. If the
; root node is not a leaf, find the height of the left and right subtrees
; and return the larger of the two, plus one for the root.
;
; LIST2BTREE: Check the length of LEAVES. If only one element, return that
; element (a valid binary tree). If two elements, return LEAVES (a valid
; binary tree already). Otherwise, split the leaves in half using SPLIT-LIST,
; and recurse on each half to make valid binary subtrees, then return a list 
; of both subtrees.
;
; BTREE2LIST: If the binary tree is a single number, return a list containing
; that number. Otherwise, recursively apply to the left and right subtrees,
; and append the two lists.
;
; IS-SAME: Check if E1 and E2 are both empty lists or the same number (base 
; case). If only one is an empty list and the other is not, return NIL. 
; Check if both are lists. If so, verify that the head elements
; are the same (via recursion) and the tails are the same (via recursion).
; If not (one is a number and one is a list), return NIL.
;
; FLATTEN-APPEND: If E1 is non-null, append it with the flattened E2 
; (so that the start of the output is exactly E1) and recurse with null E1. 
; Once E1 is null, flatten E2 into a single list. When E2 is null or a 
; single number, this is trivial. When E2 is a list, find the head. If the 
; head is a number, cons it on the flattened cdr of E2. If the head is a
; list, "flatten" E2 into a list composed of the first element of the head, 
; the tail of the head (if non-null), and the tail of E2, then recurse. This 
; accomplishes a depth-first traversal of E2, until we reach an atom 
; (number) which is consumed by cons, then continuing on the last depth 
; encountered, if there are remaining non-null elements at that level.

(defun TREE-CONTAINS (N TREE)
	; Takes a number N and an ordered tree TREE and returns T if N is in TREE,
	; NIL otherwise.
	(cond
		((numberp TREE) (= N TREE))
		((= N (cadr TREE)) 'T)
		((> N (cadr TREE)) (TREE-CONTAINS N (caddr TREE)))
		('T (TREE-CONTAINS N (car TREE)))
	)
)

(defun TREE-MAX (TREE)
	; Takes an ordered tree TREE and returns the maximum number in the tree
	(cond
		((numberp TREE) TREE)
		('T (TREE-MAX (caddr TREE)))
	)
)

(defun TREE-ORDER (TREE)
	; Takes an ordered tree TREE and returns a post-ordered list of the
	; numbers in the tree
	(cond
		((numberp TREE) (list TREE))
		('T (append (TREE-ORDER (car TREE)) 
			(TREE-ORDER (caddr TREE)) 
			(list (cadr TREE))
		))
	)
)

(defun SUB-LIST (L START LEN)
	; Takes a list L and two numbers: the element to start at (START) and
	; the number of elements (LEN). Returns a sublist of L starting at START
	; of length LEN (or less, if there are fewer than LEN elements). Indices
	; start from 0. START and LEN must be non-negative.
	(cond
		((= LEN 0) 'NIL)
		((null L) 'NIL)
		((> START 0) (SUB-LIST (cdr L) (- START 1) LEN))
		('T (cons (car L) (SUB-LIST (cdr L) START (- LEN 1))))
	)
)

(defun SPLIT-LIST (L)
	; Takes a list L and splits it into as close to equal halves as 
	; possible. The first list L1 will have at most one more element than the
	; second list L2, and at least an equal number of elements. 
	; Returns a list (L1 L2) of the two sublists.
	(let* (
		(LL (length L))
		(LEFT (cond
			((oddp LL) (+ 1 (/ (- LL 1) 2)))
			('T (/ LL 2))))
		(RIGHT (- LL LEFT)))
		(list (SUB-LIST L 0 LEFT) (SUB-LIST L LEFT RIGHT))
	)
)

(defun BTREE-HEIGHT (TREE)
	; Takes a binary tree TREE and returns the height (the length of the 
	; longest path from the root to a leaf node)
	(cond
		; Base case (atom)
		((atom TREE) 0)
		; Internal node
		('T (let ((LHEIGHT (BTREE-HEIGHT (car TREE)))
				(RHEIGHT (BTREE-HEIGHT (cadr TREE))))
				(cond
					((> LHEIGHT RHEIGHT) (+ LHEIGHT 1))
					('T (+ RHEIGHT 1))
				)
			)
		)
	)
)

(defun LIST2BTREE (LEAVES)
	; Takes a list of atoms LEAVES and returns a valid binary tree as close to
	; balanced as possible. The left subtree of each internal node will have
	; at most one more leaf than the right subtree, at least an equal number.
	; LEAVES must be non-empty.
	(let ((LEN (length LEAVES)))
		(cond
			((= LEN 1) (car LEAVES))
			((= LEN 2) LEAVES)
			('T (let ((HALVES (SPLIT-LIST LEAVES)))
					(list (LIST2BTREE (car HALVES)) 
						(LIST2BTREE (cadr HALVES)))
				)
			)
		)
	)
)

(defun BTREE2LIST (TREE)
	; Takes a binary tree TREE and produces a list of all the leaves, left to
	; right, depth-first.
	(cond
		((atom TREE) (list TREE))
		('T (append (BTREE2LIST (car TREE)) (BTREE2LIST (cadr TREE))))
	)
)

(defun IS-SAME (E1 E2)
	; Return T if Lisp expressions E1 and E2 are identical, NIL otherwise.
	; All atoms in E1 and E2 must be numbers.
	(cond
		; Both empty lists
		((and (null E1) (null E2)) 'T)
		; Mismatch - only one list is empty
		((or (null E1) (null E2)) 'NIL)
		; Both numbers
		((and (numberp E1) (numberp E2)) (= E1 E2))
		; Both lists
		((and (listp E1) (listp E2)) (and 
			(IS-SAME (car E1) (car E2)) 
			(IS-SAME (cdr E1) (cdr E2))))
		; Mismatch (list and number)
		('T 'NIL)
	)
)
		
(defun FLATTEN-APPEND (E1 E2)
	; Takes two LISP expressions E1 and E2, flattens E2 into a single list 
	; (depth-first), and appends all of E2 to E1, returning the new list.
	; E1 may not be an atom. All atoms in E1 and E2 must be numbers.
	(cond
		((null E1) (cond
			; Base cases (empty list, single number atom)
			((null E2) E2)
			((numberp E2) (cons E2 'NIL))
			; E2 is a list
			('T (let ((HEAD (car E2)))
				(cond
					; HEAD is a number, append it to the chain
					((numberp HEAD) (cons HEAD (FLATTEN-APPEND E1 (cdr E2))))
					; HEAD is a list
					((null (cdr HEAD)) (FLATTEN-APPEND E1 
						(cons (car HEAD) (cdr E2)))) ; One element list
					('T (FLATTEN-APPEND E1 (cons (car HEAD) 
						(cons (cdr HEAD) (cdr E2))))) ; Multi element list
				)
			))
		))
		('T (append E1 (FLATTEN-APPEND 'NIL E2)))
	)
)