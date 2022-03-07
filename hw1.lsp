					;TREE-CONTAINS takes two arguments N
					;and TREE, and checks whether number
					;N appears in the ordered tree TREE
(defun TREE-CONTAINS (N TREE)
  (cond ((null TREE) NIL)
	((numberp TREE) (equal N TREE))
	((< N (second TREE)) (TREE-CONTAINS N (first TREE)))
	((> N (second TREE)) (TREE-CONTAINS N (third TREE)))
	(t (equal N (second TREE)))
	)

  )

					;TREE-MIN takes one argument TREE
					;and returns the minimum number
					;appearing in the ordere tree TREE
(defun TREE-MIN (TREE)
  (cond ((null TREE) NUL)
	((atom TREE) TREE)
	(t (TREE-MAX (third TREE)))
	)
  )

					;TREE-ORDER takes one argument TREE
					;and returns a pre-ordered list
					;of the numbers appearing in the TREE
(defun TREE-ORDER (TREE)
  (cond ((null TREE) NILL)
	((atom TREE) TREE)
	(

	 
	 
