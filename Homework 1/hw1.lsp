;;; Jason Lai - 204995126
;;; CS 161 - Homework 1

;;; Solutions:
;;; 1. To compute the Nth Padovan number, the PAD function uses recursion wherein at each  
;;;    recursive step, PAD takes the sum of two recursive calls to itself with arguments 
;;;    (N-2) and (N-3), as the Nth Padovan number is the sum of the (N-2)th and (N-3)th 
;;;    Padovan numbers. These summed recursive calls continue until the base case is 
;;;    reached where PAD(0) = PAD(1) = PAD(2) = 1, and the value 1 is returned in order to 
;;;    finally compute the sums of the previous recursive steps. Once all recursive calls 
;;;    have returned their respective Padovan numbers and the values have been summed, the 
;;;    final returned value of the initial PAD call is equal to the Nth Padovan number.
;;;
;;; 2. To find the number of addition operations utilized by the PAD function to compute 
;;;    the Nth Padovan number, the SUMS function implements the same logic as the PAD 
;;;    function. However, because SUMS returns the number of addition operations and not 
;;;    actual Padovan numbers, two key changes are made in the SUMS function. First, in 
;;;    addition to the sum of the two recursive calls made at each step like in PAD, we 
;;;    also add 1 to this sum in order to increment our final return value. This comes 
;;;    from the fact that to calculate the number of addition operations needed by PAD to 
;;;    find Nth Padovan number, the Nth SUMS number is then just sum of addition operations 
;;;    used to calculate the (N-2)th and (N-3)th Padovan numbers, plus the one addition 
;;;    operation needed to add these two values together to get the Nth Padovan number.
;;;    For the second change, the SUMS base case returns 0 instead of 1, because the base 
;;;    case indicates we have reached the lowest three numbers in the Padovan sequence and 
;;;    therefore have no addition operations to execute anymore, since the lowest three 
;;;    numbers are 1's and require no addition operations. These two changes make it so 
;;;    that SUMS now returns the number of addition operations used by PAD when computing 
;;;    the Nth Padovan number, and by using the same PAD logic in SUMS, we avoid calling 
;;;    the PAD function directly as per the constraints in the spec. 
;;;
;;; 3. In order to anonymize the tree, we recursively traverse the tree, visit each node, 
;;;    and replace the atom at each node with a question mark (?). To do this, we must first 
;;;    be aware of the two possible types of arguments passed to the ANON function: atoms ('a) 
;;;    and lists ('(a)). If just an atom is passed to ANON, then a single question mark is 
;;;    returned, and if a list is passed to ANON, then we must recursively traverse the list 
;;;    and any possible lists within the list. So, ANON will essentially have two possible 
;;;    types of returns depending on the type of its argument TREE: if TREE is a list, ANON 
;;;    returns a list, and if TREE is an atom, ANON returns an atom. The following paragraphs 
;;;    explain my solution by discussing these two cases separately.
;;;
;;;    Starting with the atom case, the second predicate of the cond in the ANON function 
;;;    tests directly if the argument TREE is a single atom not contained within a list. If 
;;;    so, then ANON simply returns a question mark atom ('?). However, it's important that 
;;;    this predicate for testing TREE as an atom comes after the base case for the recursion 
;;;    if TREE is a list. I will explain why in the last paragraph. 
;;;
;;;    Now with the list case, if the argument TREE is a list, we have to differentiate between 
;;;    atoms and other lists within TREE. To do this, the third predicate tests to see if the 
;;;    first element in TREE is an atom using "atom" and "car". If the first element is in fact 
;;;    an atom, then we append a list with a single question mark '(?) to the return of a 
;;;    recursive call to ANON that takes in the rest of the TREE using "cdr". What this is 
;;;    essentially doing is adding an anonymized node to a list that will eventually be a tree 
;;;    with the same format as TREE. However, if the first element in TREE is not an atom, then 
;;;    it's a list, and we need to anonymize the atoms of this "inner" list. So, the last 
;;;    predicate does this by appending a call to ANON on this "inner" list with another call 
;;;    to ANON with the rest of TREE. However, it's important to note that we must wrap the call 
;;;    to ANON on the inner list with a "list" so that append treats this inner list as one whole 
;;;    element when appending it to the final anonymized tree. These recursive calls continue 
;;;    until the base case is reached, wherein the TREE argument is now null, meaning that we've 
;;;    reached the end of the list of the original TREE argument. The base case then returns an 
;;;    empty list, which is appended to the final tree, which has no effect, and the final tree 
;;;    should have the same format as the original argument TREE at this point.
;;;
;;;    Going back to the atom case, the second predicate that tests if TREE is a single atom 
;;;    must come after the base case of the list recursion. This is because if the null test 
;;;    comes after the atom test, an empty list '() evaluates to true as an atom. So, when a list 
;;;    recursion reaches the end of TREE, the empty list will see the atom predicate first and 
;;;    evaluate to true, thereby returning a question mark atom instead of an empty list at the 
;;;    base case. This question mark atom would then be appended to the end of our final 
;;;    anonymized tree and create a dotted list with an extra question mark at the end, which is 
;;;    incorrect.

;;; PAD takes a single integer argument N and returns the Nth Padovan number. The Nth 
;;; Padovan number corresponds to the sum of the (N-2)th and (N-3)th Padovan numbers.
(defun PAD (N) (cond 
			((<= N 2) 1) 
			(t (+ (PAD (- N 2)) (PAD (- N 3))))
		)
)

;;; SUMS takes a single integer argument N and returns the number of addition operations
;;; required by the algorithm of the function PAD to compute the Nth Padovan number.
(defun SUMS (N) (cond 
			((<= N 2) 0) 
			(t (+ 1 (+ (SUMS (- N 2)) (SUMS (- N 3)))))
		)
)

;;; ANON takes a single argument TREE that represents a tree using atoms as nodes and lists as 
;;; branches, and returns the same tree in the same format, but with all of its nodes (atoms) 
;;; replaced with question marks (?), thereby anonymizing TREE.
(defun ANON (TREE) (cond 
			((null TREE) '())
			((atom TREE) '?) 
			((atom (car TREE)) (append '(?) (ANON (cdr TREE))))   
			(t (append (list (ANON (car TREE))) (ANON (cdr TREE))))
		)
)