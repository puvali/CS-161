;BFS takes a single argument that is the list representation of the tree
;and returns a single, top-level list of the terminal nodes in the order
;they would be visited by a left-to-right breadth-first search
;(BFS '((A (B)) C (D))) returns (C A D B)
(defun BFS (tree)
  (cond
   ((null tree) NIL)
   ((listp (car tree)) (BFS (append (cdr tree) (car tree))))
   ((atom (car tree)) (append (list (car tree)) (BFS (cdr tree))))))



;DFS takes a single argument that is the list representation of the tree
;and returns a single top-level list of the terminal nodes in the order
;they would be visited by a right-to-left depth-first search
;(DFS '((A (B)) C (D))) returns (D C B A)
(defun DFS (tree)
  (cond
   ((null tree) NIL)
   ((atom tree) (list tree))
   (t (append (DFS (cdr tree)) (DFS (car tree))))))



;DFID takes two arguments: the list representation of the tree
;and an integer representing the maximum depth of the
;and returns a single top-level list of the terminal nodes in the
;order that they would be visited by a left-to-right
;depth-first iterative-deepening search.
;DFID-helper assists DFID by performing the depth-first iterative
;deepening search but it only searches up to the given depth
(defun DFID (tree depth)
  (cond
   ((null tree) NIL)
   ((equal depth 0) NIL)
   ((< depth 0) NIL)
   (t (append (DFID tree (- depth 1)) (DFID-helper tree depth)))))

(defun DFID-helper (tree depth)
  (cond
   ((null tree) NIL)
   ((equal depth 0) NIL)
   ((< depth 0) NIL)   
   ((atom tree) (list tree))
   (t (append (DFID-helper (car tree) (- depth 1)) (DFID-helper (cdr tree) depth)))))




;******************* CODE SKELETON FOR MISSIONARY-CANNIBAL PROBLEM *************
; These functions implement a depth-first solver for the missionary-cannibal
; problem. In this problem, three missionaries and three cannibals are trying to
; go from the east side of a river to the west side. They have a single boat
; that can carry two people at a time from one side of the river to the
; other. There must be at least one person in the boat to cross the river. There
; can never be more cannibals on one side of the river than missionaries. If
; there are, the cannibals eat the missionaries.

; In this implementation, a state is represented by a single list
; (missionaries cannibals side). side represents which side the boat is
; currently on, and is T if it is on the east side and NIL if on the west
; side. missionaries and cannibals represent the number of missionaries and
; cannibals on the same side as the boat. Thus, the initial state for this
; problem is (3 3 T) (three missionaries, three cannibals, and the boat are all
; on the east side of the river) and the goal state is (3 3 NIL).

; The main entry point for this solver is the function MC-DFS, which is called
; with the initial state to search from and the path to this state. It returns
; the complete path from the initial state to the goal state: this path is a
; list of intermediate problem states. The first element of the path is the
; initial state and the last element is the goal state. Each intermediate state
; is the state that results from applying the appropriate operator to the
; preceding state. If there is no solution, MC-DFS returns NIL.

; To call MC-DFS to solve the original problem, one would call (MC-DFS '(3 3 T)
; NIL) -- however, it would be possible to call MC-DFS with a different initial
; state or with an initial path.

; Examples of calls to some of the helper functions can be found after the code.



; FINAL-STATE takes a single argument s, the current state, and returns T if it
; is the goal state (3 3 NIL) and NIL otherwise.
(defun final-state (s)
  (equal s '(3 3 NIL)))

; NEXT-STATE returns the state that results from applying an operator to the
; current state. It takes three arguments: the current state (s), a number of
; missionaries to move (m), and a number of cannibals to move (c). It returns a
; list containing the state that results from moving that number of missionaries
; and cannibals from the current side of the river to the other side of the
; river. If applying this operator results in an invalid state (because there
; are more cannibals than missionaries on either side of the river, or because
; it would move more missionaries or cannibals than are on this side of the
; river) it returns NIL.
;
; NOTE that next-state returns a list containing the successor state (which is
; itself a list); the return should look something like ((1 1 T)).
(defun next-state (s m c)
  (let*
      ((from_side (list (- (first s) m) (- (second s) c)))
       (to_side (list (+ (- 3 (first s)) m) (+ (- 3 (second s)) c))))
    (cond
     ((> m (first s)) NIL)
     ((> c (second s)) NIL)
     ((and (> (first from_side) 0) (> (second from_side) (first from_side))) NIL)
     ((and (> (first to_side) 0) (> (second to_side) (first to_side))) NIL)
     (t (list (list (first to_side) (second to_side) (not (third s))))))))

; SUCC-FN returns all of the possible legal successor states to the current
; state. It takes a single argument (s), which encodes the current state, and
; returns a list of each state that can be reached by applying legal operators
; to the current state.
(defun succ-fn (s)
  (append
   (next-state s 1 0)
   (next-state s 2 0)
   (next-state s 1 1)
   (next-state s 0 1)
   (next-state s 0 2)))

; ON-PATH checks whether the current state is on the stack of states visited by
; this depth-first search. It takes two arguments: the current state (s) and the
; stack of states visited by MC-DFS (states). It returns T if s is a member of
; states and NIL otherwise.
(defun on-path (s states)
  (cond
   ((equal (length states) 0) NIL)
   ((equal s (first states)) t)
   (t (on-path s (rest states)))))

; MULT-DFS is a helper function for MC-DFS. It takes two arguments: a stack of
; states from the initial state to the current state (path), and the legal
; successor states from the current state (states).
; MULT-DFS does a depth-first search on each element of states in
; turn. If any of those searches reaches the final state, MULT-DFS returns the
; complete path from the initial state to the goal state. Otherwise, it returns
; NIL. 
; Note that the path should be ordered as: (S_n ... S_2 S_1 S_0)
(defun mult-dfs (states path)
  (cond
   ((equal (length states) 0) NIL)
   ((mc-dfs (first states) path) (mc-dfs (first states) path))
   (t (mult-dfs (rest states) path))))

; MC-DFS does a depth first search from a given state to the goal state. It
; takes two arguments: a state (S) and the path from the initial state to S
; (PATH). If S is the initial state in our search, PATH should be NIL. MC-DFS
; performs a depth-first search starting at the given state. It returns the path
; from the initial state to the goal state, if any, or NIL otherwise. MC-DFS is
; responsible for checking if S is already the goal state, as well as for
; ensuring that the depth-first search does not revisit a node already on the
; search path.
(defun mc-dfs (s path)
  (cond
   ((final-state s) (append path (list s)))
   ((not (on-path s path)) (mult-dfs (succ-fn s) (append path (list s))))
   (t NIL)))



; Function execution examples

; Applying this operator would result in an invalid state, with more cannibals
; than missionaries on the east side of the river.
; (next-state '(3 3 t) 1 0) -> NIL

; Applying this operator would result in one cannibal and zero missionaries on
; the west side of the river, which is a legal operator. (NOTE that next-state
; returns a LIST of successor states, even when there is only one successor)
; (next-state '(3 3 t) 0 1) -> ((0 1 NIL))

; succ-fn returns all of the legal states that can result from applying
; operators to the current state.
; (succ-fn '(3 3 t)) -> ((0 1 NIL) (1 1 NIL) (0 2 NIL))
; (succ-fn '(1 1 t)) -> ((3 2 NIL) (3 3 NIL))
