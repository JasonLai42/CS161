;
; Graph coloring to SAT conversion
;
; All functions you need to write are marked with 'EXERCISE' in their header comments.
; Same rules apply regarding Lisp functions you are allowed to use.
; In fact, you do not need a lot of Lisp functions to finish this assignment.
;

;;;;;;;;;;;;;;;;;;;;;;
; General util.
;
(defun reload()
  (load "hw4.lsp")
  );end defun

; EXERCISE: Fill this function.
; returns the index of the variable
; that corresponds to the fact that 
; "node n gets color c" (when there are k possible colors).
;
;;; node2var is a function that takes in a node index n, a color index c, and a maximum color index 
;;; k. node2var returns a value that represents the assignment of color c to node n via the 
;;; expression (n-1) * k + c.
(defun node2var (n c k)
  (+ c 
    (* k 
      (- n 1)))
)

; EXERCISE: Fill this function
; returns *a clause* for the constraint:
; "node n gets at least one color from the set {c,c+1,...,k}."
;
;;; at-least-one-color is a function that takes in a node index n, a color index c, and a maximum 
;;; color index k. at-least-one-color then returns a clause that says node n gets at least one color 
;;; from the set of colors from c to k inclusive. To do this, we simply get every possible assignment 
;;; of color to n and compile them into a list by using cons and node2var.
(defun at-least-one-color (n c k)
  (cond 
    ((> c k) '())
    (t (cons (node2var n c k) (at-least-one-color n (+ c 1) k)))
  )
)

; EXERCISE: Fill this function
; returns *a list of clauses* for the constraint:
; "node n gets at most one color from the set {c,c+1,...,k}."
;
;;; at-most-one-color is a function that takes in a node index n, a color index c, and a maximum 
;;; color k. at-most-one-color then returns a list of clauses that say node n can only be assigned 
;;; at most one color from the set of colors from c to k inclusive. To do this, at-most-one-color 
;;; calls its helper function c-to-k which basically takes two consecutive color indices c and c+1 
;;; and iterates from c+1 to k, while generating clauses that say node n cannot have the color c and 
;;; whatever iteration of color index i we are currently on. Once this list is returned, it is 
;;; appended to a list of the other clauses for other color indices c.
(defun at-most-one-color (n c k)
  (cond 
    ((>= c k) '())
    (t (append (c-to-k n c (+ c 1) k) (at-most-one-color n (+ c 1) k)))
  )
)

;;; c-to-k is a helper function for at-most-one-color that takes in four arguments: a node index n, 
;;; a color index c, the color index d that is one higher than c, and the maximum color index k. 
;;; c-to-k returns a list of clauses that say that node n cannot have another color in addition to 
;;; the color c, without repeating clauses already added to the list before i.e. (-25, -24) won't 
;;; be added if (-24, -25) was already added.
(defun c-to-k (n c d k) 
  (cond 
    ((> d k) '())
    (t (cons (negate n c d k) (c-to-k n c (+ d 1) k)))
  )
)

;;; negate is a helper function for c-to-k that takes in the same four arguments: a node index n, 
;;; a color index c, and the color index d that is one higher than c, and the maximum color index k. 
;;; negate returns a clause that is the negation of the value if c is assigned to n and the negation 
;;; of the value if d is assigned to n.
(defun negate (n c d k)
  (list 
    (* -1 (node2var n c k))
    (* -1 (node2var n d k))
  )
)

; EXERCISE: Fill this function
; returns *a list of clauses* to ensure that
; "node n gets exactly one color from the set {1,2,...,k}."
;
;;; generate-node-clauses is a function that takes in a node index n and a maximum color index k and 
;;; returns a list of clauses that essentially say that node n gets exactly one color from the set 
;;; 1 to k inclusive. To do this, we get a list of clauses specifying that node n gets at most one 
;;; color by calling at-most-one-color, then cons the clause that says node n gets at least one color 
;;; by calling at-least-one-color.
(defun generate-node-clauses (n k)
  (cons (at-least-one-color n 1 k) (at-most-one-color n 1 k))
)

; EXERCISE: Fill this function
; returns *a list of clauses* to ensure that
; "the nodes at both ends of edge e cannot have the same color from the set {1,2,...,k}."
;
;;; generate-edge-clauses is a function that takes in an edge e (pair of two node indices) and a 
;;; maximum color index k and returns a list of clauses that say that the two nodes connected by 
;;; edge e cannot have the same color. To do this, generate-edge-clauses calls its helper 
;;; gen-edge-helper that takes the lists of possible color assignments for either node, then returns 
;;; the list of clauses that say both nodes cannot have the same color, which is essentially iterating 
;;; through both lists and pairing the negative values of both lists that the current iteration is on.
(defun generate-edge-clauses (e k)
  (gen-edge-helper (at-least-one-color (car e) 1 k) (at-least-one-color (cadr e) 1 k))
)

;;; gen-edge-helper is the helper function for generate-edge-clauses that takes in two 
;;; arguments: a list of possible color assignments for one node index l1, and another list of 
;;; possible color assignments for another node index l2 (both ends of an edge). gen-edge-helper 
;;; will return a list of clauses that say that the node represented by l1 and the node represented 
;;; by l2 cannot have the same color.
(defun gen-edge-helper (l1 l2)
  (cond 
    ((null l1) '())
    (t (cons (negate-nodes (car l1) (car l2)) (gen-edge-helper (cdr l1) (cdr l2))))
  )
)

;;; negate-nodes is the helper function for gen-edge-helper that takes in two arguments: an 
;;; assignment of color index to a node index n1, and another assignment of color index to a node 
;;; index n2. negate-nodes will return a clause that has both values negated to represent that 
;;; an assignment like this: both n1 and n2 have same color is not valid.
(defun negate-nodes (n1 n2) 
  (list 
    (* -1 n1)
    (* -1 n2)
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Your exercises end here. Below are top-level
; and utility functions that you do not need to understand.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; 
; Top-level function for converting the graph coloring problem
; of the graph defined in 'fname' using k colors into a SAT problem.
; The resulting SAT problem is written to 'out-name' in a simplified DIMACS format.
; (http://www.satcompetition.org/2004/format-solvers2004.html)
;
; This function also returns the cnf written to file.
; 
; *works only for k>0*
;
(defun graph-coloring-to-sat (fname out-name k)
  (progn
    (setf in-path (make-pathname :name fname))
    (setf in (open in-path :direction :input))
    (setq info (get-number-pair-from-string (read-line in) #\ ))
    (setq cnf nil)
    (do ((node 1
	       (+ node 1)
	       ))
	((> node (car info)))
      (setq cnf (append (generate-node-clauses node k) cnf))
      );end do
    (do ((line (read-line in nil 'eof)
	       (read-line in nil 'eof)))
	((eql line 'eof) (close in))
      (let ((edge (get-number-pair-from-string line #\ )))
	(setq cnf (append (generate-edge-clauses edge k) cnf))
	);end let
      );end do
    (close in)
    (write-cnf-to-file out-name (* (car info) k) cnf)
    (return-from graph-coloring-to-sat cnf)
    );end progn  
  );end defun

;
; A utility function for parsing a pair of integers.
; 
(defun get-number-pair-from-string (string token)
  (if (and string token)
      (do* ((delim-list (if (and token (listp token)) token (list token)))
            (char-list (coerce string 'list))
            (limit (list-length char-list))
            (char-count 0 (+ 1 char-count))
            (char (car char-list) (nth char-count char-list))
            )
           ((or (member char delim-list)
                (= char-count limit))
            (return
               (if (= char-count limit)
                   (list string nil)
                   (list (parse-integer (coerce (butlast char-list (- limit char-count))
                                 'string))
                         (parse-integer (coerce (nthcdr (+ char-count 1) char-list) 'string))
			 )))))))

;
; Writes clause to file handle 'out'.
;
(defun write-clause-to-file (out clause)
  (cond ((null clause) (format out "0~%"))
	(t (progn 
	     (format out "~A " (car clause))
	     (write-clause-to-file out (cdr clause))
	     );end progn
	   );end t
	);end cond
  );end defun

;
; Writes the formula cnf with vc variables to 'fname'.
;
(defun write-cnf-to-file (fname vc cnf)
  (progn
    (setf path (make-pathname :name fname))
    (setf out (open path :direction :output))
    (setq cc (length cnf))  
    (format out "p cnf ~A ~A~%" vc cc)
    (dolist (clause cnf)
      (write-clause-to-file out clause)
      );end dolist
    (close out)
    );end progn
  );end defun
