;;; Jason Lai - 204995126
;;; CS 161 - *Note: Comments preceded by 3 semicolons are my comments.

;;;;;;;;;;;;;;
; Homework 2 ;
;;;;;;;;;;;;;;

; TODO: comment code
;;; TA on Piazza mentioned not having to include header comment for the functions of problem 2 
;;; since they are already done for us in the skeleton.

;;; Solutions: 
;;; 1. 
;;;     BFS:
;;;         BFS will essentially perform a breadth-first search on a tree represented as a list, 
;;;         which entails returning a list of leaf nodes in order from the highest level of 
;;;         the tree down to the lowest level, in a left-to-right precedence. This means atoms 
;;;         we encounter at the current level we are expanding have to be placed in the final 
;;;         list before any of the nodes in the lower levels. So, to perform the BFS we traverse 
;;;         the list recursively by visiting each element of the list in order and append atoms 
;;;         to the final returned list in the order that we encounter them. To do this, the 
;;;         second predicate handles this by appending the atom we just in encountered in front 
;;;         of all nodes and subtrees we have yet to encounter, and because of the nature of 
;;;         the recursion, all nodes we encountered at higher levels of the tree before the  
;;;         current node are in front of the current node. Then to expand subtrees, since the 
;;;         subtrees are essentially lists within the list that is FRINGE, we have to unwrap the 
;;;         outer list of the subtree to access the nodes that are at the highest level of that 
;;;         subtree. To do this, the third predicate is an else statement (since if the element 
;;;         we encountered wasn't an atom, it's a list) that appends the list that represents 
;;;         the subtree to the end of FRINGE and calls BFS on the new FRINGE. What this 
;;;         essentially does is unwrap the outer list of the subtree by merging the subtree 
;;;         list at the back of FRINGE, which reveals the atoms at the top level of the subtree, 
;;;         and since it's been appended to back of the list, we will encounter the atoms in 
;;;         the correct order for BFS. The base case for this recursion is just when FRINGE 
;;;         is an empty list, signifying that the open list of nodes is now empty.
;;; 2.
;;;     FINAL-STATE: 
;;;         FINAL-STATE returns T if we've reached the goal state (T T T T), so we simply have a 
;;;         predicate that tests if the argument state S is equal to '(T T T T). If it is, then 
;;;         FINAL-STATE returns true, otherwise, it returns NIL.
;;;     NEXT-STATE: 
;;;         NEXT-STATE determines whether the given action A for the given state S results in a 
;;;         valid state, and will return that state if true. So essentially, the NEXT-STATE 
;;;         algorithm defines rules for each action in order for that action to result in a 
;;;         valid state. An overall cond expression first evaluates what action A is then 
;;;         moves onto a nested cond that determines what the requirements are for that 
;;;         specific action to result in a valid state. Note that given how the algorithm works, 
;;;         it's impossible for the baby and dog or baby and poison to be on the side opposite 
;;;         from Homer.
;;;
;;;         For action h where Homer moves alone, the requirements are that the baby and the dog 
;;;         are not on the same side or that the baby and the poison are not on the same side. 
;;;         To test these conditions, we check that the values at the index for each object are 
;;;         not equal using accessors such as car, cadr, etc, meaning that these objects are not 
;;;         on the same side. If so, then the action results in a valid state and we perform the 
;;;         move. To do this, we first check which side Homer is on by comparing the value at 
;;;         Homer's index with NIL. If his position is NIL, we return a list where his position 
;;;         is now T, and if his position is not NIL, then it's T and we return a list where his 
;;;         position is now NIL. If a condition was not met, we return NIL.
;;;
;;;         For action b where Homer moves with the baby, since Homer is with the baby, we do 
;;;         not need to worry about the positions of the poison and the dog. The only condition 
;;;         we must meet is that Homer and the Baby are on the same side to perform this action. 
;;;         To check this, we simply check if the values are the index for Homer and the baby 
;;;         are equal, and if so, then we return a list where their positions are now switched 
;;;         to the other side. If a condition was not met, we return NIL.
;;;
;;;         For action d where Homer moves with the dog, we first check that Homer and the dog 
;;;         are on the same side, then check that the baby and the poison are not on the same 
;;;         side. If these conditions are met, we perform the move and return a list where 
;;;         the positions of Homer and the dog are now switched to the opposite side. If a 
;;;         condition was not met, we return NIL.
;;;
;;;         For action p where Homer moves with the poison, we first check that Homer and the 
;;;         poison are on the same side, then check that the baby and the dog are not on the 
;;;         same side. If these conditions are met, we perform the move and return a list where
;;;         the positions of Homer and the poison are now switched to the other side. If a 
;;;         condition was not met, we return NIL.
;;;     SUCC-FN:
;;;         SUCC-FN returns a list of all of the legal successor states to the state S. To do 
;;;         this, a let is used to assign calls to NEXT-STATE to variables for clarity. 
;;;         NEXT-STATE is called with each of the 4 different actions and the returns are then 
;;;         appended to each other. However, each of the returns of NEXT-STATE for each action 
;;;         are evaluated by a cond expression inside the append. If the return from 
;;;         NEXT-STATE for a certain action is NIL, then that means the successor state for 
;;;         that action is illegal, so we don't include it and append an empty list. If the 
;;;         return from NEXT-STATE was not NIL, then the successor state is legal and we 
;;;         append that state as an element to the final list of all legal successor states to 
;;;         S that we return.
;;;     ON-PATH: 
;;;         ON-PATH simply checks if S is a member of STATES, so to implement this, we traverse 
;;;         STATES recursively and check if the frontmost element of STATES is equal to S. If 
;;;         we encounter an element that's equal to S, we return T; if not, we continue to 
;;;         to call ON-PATH on the remaining portion of STATES that has yet to be traversed. 
;;;         The base case checks when we've reached the end of STATES and there are no more 
;;;         elements in the list, therefore we return NIL as we've traversed the whole list 
;;;         without every finding a matching element for S.
;;;     MULT-DFS:
;;;         Since DFS initially takes in a single state that we are currently at, then as 
;;;         a helper function, MULT-DFS is responsible for exploring every possible, valid 
;;;         path for the current state to find a solution. In order to do this, we have that 
;;;         the arguments to MULT-DFS as the list of states we have had so far from the initial 
;;;         state to the current state in PATH, and the list of all valid successor states to 
;;;         the current state in STATES. Since we must perform a depth-first search on each of 
;;;         the successor STATES, we simply call DFS on each element of STATES, one by one. To 
;;;         do this, we use let to set a variable called expansion to the call of DFS on the 
;;;         first element in STATES. We then use a cond expression to check if that DFS call 
;;;         resulted in NIL, meaning that that successor state failed to yield a path to the 
;;;         goal state, in which case, we perform recursion and have MULT-DFS call itself with 
;;;         the same PATH argument, but with the rest of the list of states in STATES that we 
;;;         have not visited yet. However, if the DFS call on a successor state did not return 
;;;         NIL, then that means DFS returned a list that represents the path from the initial 
;;;         state to the goal state through the successor state we called DFS on, in which case, 
;;;         MULT-DFS simply returns this list as the solution.
;;;     DFS: 
;;;         DFS performs a depth-first search on the given current state argument S with the 
;;;         given PATH argument that is a list of states from the initial state to the state 
;;;         before S (revision of spec confirmed on Piazza). DFS also checks if S is a state 
;;;         that's already been visited in PATH and if S is the goal state. To implement DFS, we 
;;;         use a cond expression where the first predicate checks if S is a state that's 
;;;         already been visited. To do this, we call the ON-PATH function with S and PATH as 
;;;         its arguments. If ON-PATH returns T then that means S is a state we've already 
;;;         visited, so DFS returns NIL. If S is not a state we've already visited, then the 
;;;         second predicate checks if S is the goal state using the FINAL-STATE function. If 
;;;         the FINAL-STATE function returns T, then the solution has been found, so DFS returns 
;;;         PATH with S appended to the end. If neither of these predicates are true, then we 
;;;         continue the depth-first search by expanding S; to do this, we call MULT-DFS with 
;;;         its arguments being the valid successor states to S by calling SUCC-FN, and PATH 
;;;         with S appended to the end. What this basically creates is a chain of expanding and 
;;;         checking between MULT-DFS and DFS to establish the depth-first search algorithm; 
;;;         both functions return a path from the initial state to the goal state if it exists 
;;;         until the originating DFS call finally returns the solution.

;;;;;;;;;;;;;;
; Question 1 ;
;;;;;;;;;;;;;;


;;; BFS takes a single argument FRINGE that is a list of search trees, in which each nest list 
;;; represents a lower level of the search tree. BFS then traverses FRINGE and returns a top 
;;; level list of the leaf nodes (atoms) in FRINGE in the order that they were encountered 
;;; top-down, left-right order of precedence. That is, the nodes are listed in order from 
;;; the highest level to the lowest level of the search trees in FRINGE, and nodes of the 
;;; same level are listed in left-to-right order.
(defun BFS (FRINGE) 
    (cond
        ((null FRINGE) '())
        ((atom (car FRINGE)) (append (list (car FRINGE)) (BFS (cdr FRINGE))))
        (t (BFS (append (cdr FRINGE) (car FRINGE))))
    )
)

;;;;;;;;;;;;;;
; Question 2 ;
;;;;;;;;;;;;;;


; These functions implement a depth-first solver for the homer-baby-dog-poison
; problem. In this implementation, a state is represented by a single list
; (homer baby dog poison), where each variable is T if the respective entity is
; on the west side of the river, and NIL if it is on the east side.
; Thus, the initial state for this problem is (NIL NIL NIL NIL) (everybody 
; is on the east side) and the goal state is (T T T T).

; The main entry point for this solver is the function DFS, which is called
; with (a) the state to search from and (b) the path to this state. It returns
; the complete path from the initial state to the goal state: this path is a
; list of intermediate problem states. The first element of the path is the
; initial state and the last element is the goal state. Each intermediate state
; is the state that results from applying the appropriate operator to the
; preceding state. If there is no solution, DFS returns NIL.
; To call DFS to solve the original problem, one would call 
; (DFS '(NIL NIL NIL NIL) NIL) 
; However, it should be possible to call DFS with a different initial
; state or with an initial path.

; First, we define the helper functions of DFS.

; FINAL-STATE takes a single argument S, the current state, and returns T if it
; is the goal state (T T T T) and NIL otherwise.
(defun FINAL-STATE (S) 
    (cond 
        ((equal S '(T T T T)) t)
        (t nil)
    )
)

; NEXT-STATE returns the state that results from applying an operator to the
; current state. It takes three arguments: the current state (S), and which entity
; to move (A, equal to h for homer only, b for homer with baby, d for homer 
; with dog, and p for homer with poison). 
; It returns a list containing the state that results from that move.
; If applying this operator results in an invalid state (because the dog and baby,
; or poisoin and baby are left unsupervised on one side of the river), or when the
; action is impossible (homer is not on the same side as the entity) it returns NIL.
; NOTE that next-state returns a list containing the successor state (which is
; itself a list); the return should look something like ((NIL NIL T T)).
(defun NEXT-STATE (S A)
    (cond 
        ((equal A 'h) 
            (cond 
                ((equal (cadr S) (caddr S)) nil)
                ((equal (cadr S) (cadddr S)) nil)
                ((equal (car S) 'NIL) (cons 'T (cdr S)))
                (t (cons 'NIL (cdr S)))))
        ((equal A 'b) 
            (cond 
                ((not (equal (car S) (cadr S))) nil)
                ((equal (car S) 'NIL) (append '(T T) (cddr S)))
                (t (append '(NIL NIL) (cddr S)))))
        ((equal A 'd) 
            (cond 
                ((not (equal (car S) (caddr S))) nil)
                ((equal (cadr S) (cadddr S)) nil)
                ((equal (car S) 'NIL) (list 'T (cadr S) 'T (cadddr S)))
                (t (list 'NIL (cadr S) 'NIL (cadddr S)))))
        ((equal A 'p) 
            (cond 
                ((not (equal (car S) (cadddr S))) nil)
                ((equal (cadr S) (caddr S)) nil)
                ((equal (car S) 'NIL) (list 'T (cadr S) (caddr S) 'T))
                (t (list 'NIL (cadr S) (caddr S) 'NIL))))
    )
)

; SUCC-FN returns all of the possible legal successor states to the current
; state. It takes a single argument (s), which encodes the current state, and
; returns a list of each state that can be reached by applying legal operators
; to the current state.
(defun SUCC-FN (S) 
    (let 
        ((homer (NEXT-STATE S 'h)) 
         (baby (NEXT-STATE S 'b))
         (dog (NEXT-STATE S 'd))
         (poison (NEXT-STATE S 'p)))
                (append 
                    (cond 
                        ((equal homer nil) '())
                        (t (list homer)))
                    (cond 
                        ((equal baby nil) '())
                        (t (list baby)))
                    (cond 
                        ((equal dog nil) '())
                        (t (list dog)))
                    (cond 
                        ((equal poison nil) '())
                        (t (list poison))))
    )
)

; ON-PATH checks whether the current state is on the stack of states visited by
; this depth-first search. It takes two arguments: the current state (S) and the
; stack of states visited by DFS (STATES). It returns T if s is a member of
; states and NIL otherwise.
(defun ON-PATH (S STATES)
    (cond 
        ((null STATES) nil)
        ((equal S (car STATES)) t)
        (t (ON-PATH S (cdr STATES)))
    )
)

; MULT-DFS is a helper function for DFS. It takes two arguments: a list of
; states from the initial state to the current state (PATH), and the legal
; successor states to the last, current state in the PATH (STATES). PATH is a
; first-in first-out list of states; that is, the first element is the initial
; state for the current search and the last element is the most recent state
; explored. MULT-DFS does a depth-first search on each element of STATES in
; turn. If any of those searches reaches the final state, MULT-DFS returns the
; complete path from the initial state to the goal state. Otherwise, it returns
; NIL.
(defun MULT-DFS (STATES PATH)
    (let 
        ((expansion (DFS (car STATES) PATH)))
                (cond 
                    ((equal expansion nil) (MULT-DFS (cdr STATES) PATH))
                    (t expansion))
    )
)

; DFS does a depth first search from a given state to the goal state. It
; takes two arguments: a state (S) and the path from the initial state to S
; (PATH). If S is the initial state in our search, PATH is set to NIL. DFS
; performs a depth-first search starting at the given state. It returns the path
; from the initial state to the goal state, if any, or NIL otherwise. DFS is
; responsible for checking if S is already the goal state, as well as for
; ensuring that the depth-first search does not revisit a node already on the
; search path.
(defun DFS (S PATH)
    (cond 
        ((ON-PATH S PATH) nil)
        ((FINAL-STATE S) (append PATH (list S)))
        (t (MULT-DFS (SUCC-FN S) (append PATH (list S))))
    )
)