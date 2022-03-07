;;;;;;;;;;;;;;
; Homework 4 ;
;;;;;;;;;;;;;;

					; checks to see whether a literal is in the assignment list and returns
					; either true or NIL depending on whether it is a positive or negative literal respectively
					; assigns the variable and returns true if the literal is not the list
(defun check-literal (assign literal)
  (let* ((value (car assign)))
    (cond
     ((null assign) t)
     ((= literal value) t)
     ((= (+ literal value) 0) NIL)
     (t (check-literal (cdr assign) literal)))))

					; checks whether a clause is satisfied based on the assigned variables
					; or if it can be satisfied by assigning values to variables
(defun check-clause (assign clause)
  (if (null clause)
    NIL
    (cond
     ((check-literal assign (car clause)) t)
     (t (check-clause assign (cdr clause))))))

					; checks to see whether the clause list is satisfied based on the assigned
					; variables or whether it can be satisfied by assigning values to variables
(defun check-clauselist (assign clauselist)
  (if (null clauselist)
      t (and (check-clause assign (car clauselist)) (check-clauselist assign (cdr clauselist)))))

					; assigns either a +1 or -1 sign to the next variable in the assignment lsit
(defun assign-newvar (assign sign)
  (let* ((var (+ (length assign) 1)))
    (if (= sign 1)
      (append assign (list var))
      (append assign (list (- 0 var))))))

					; the recursive backtracking search algorithm checks whether the assignment list is valid
					; if yes and the list is fully assigned, return list
					; else, performs depth first search and attempts assigning t/f value to unassigned variables
(defun rec-backtr (n assign clauselist)
  (if (check-clauselist assign clauselist)
    (if (= (length assign) n)
	  assign (or (rec-backtr n (assign-newvar assign 1) clauselist) (rec-backtr n (assign-newvar assign -1) clauselist)))
    NIL))

; EXERCISE: Modify this function to decide satisifiability of delta.
; If delta is satisfiable, sat? returns a list of n integers that represents a model of delta,  
; otherwise it returns NIL. (See spec for details.)
; param n: number of variables in delta
; param delta: a CNF represented as a list of lists
(defun sat? (n delta)
  (rec-backtr n '() delta))

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

