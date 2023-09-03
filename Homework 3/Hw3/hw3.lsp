;;; Jason Lai - 204995126
;;; CS 161 - *Note: Comments preceded by 3 semicolons are my comments.
;
; CS161 Hw3: Sokoban
; 
; *********************
;    READ THIS FIRST
; ********************* 
;
; All functions that you need to modify are marked with 'EXERCISE' in their header comments.
; Do not modify a-star.lsp.
; This file also contains many helper functions. You may call any of them in your functions.
;
; *Warning*: The provided A* code only supports the maximum cost of 4999 for any node.
; That is f(n)=g(n)+h(n) < 5000. So, be careful when you write your heuristic functions.
; Do not make them return anything too large.
;
; For Allegro Common Lisp users: The free version of Allegro puts a limit on memory.
; So, it may crash on some hard sokoban problems and there is no easy fix (unless you buy 
; Allegro). 
; Of course, other versions of Lisp may also crash if the problem is too hard, but the amount
; of memory available will be relatively more relaxed.
; Improving the quality of the heuristic will mitigate this problem, as it will allow A* to
; solve hard problems with fewer node expansions.
; 
; In either case, this limitation should not significantly affect your grade.
; 
; Remember that most functions are not graded on efficiency (only correctness).
; Efficiency can only influence your heuristic performance in the competition (which will
; affect your score).
;  
;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; General utility functions
; They are not necessary for this homework.
; Use/modify them for your own convenience.
;

;
; For reloading modified code.
; I found this easier than typing (load "filename") every time. 
;
(defun reload()
  (load "hw3.lsp")
  )

;
; For loading a-star.lsp.
;
(defun load-a-star()
  (load "a-star.lsp"))

;
; Reloads hw3.lsp and a-star.lsp
;
(defun reload-all()
  (reload)
  (load-a-star)
  )

;
; A shortcut function.
; goal-test and next-states stay the same throughout the assignment.
; So, you can just call (sokoban <init-state> #'<heuristic-name>).
; 
;
(defun sokoban (s h)
  (a* s #'goal-test #'next-states h)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; end general utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; We now begin actual Sokoban code
;

; Define some global variables
(setq blank 0)
(setq wall 1)
(setq box 2)
(setq keeper 3)
(setq star 4)
(setq boxstar 5)
(setq keeperstar 6)

; Some helper functions for checking the content of a square
(defun isBlank (v)
  (= v blank)
  )

(defun isWall (v)
  (= v wall)
  )

(defun isBox (v)
  (= v box)
  )

(defun isKeeper (v)
  (= v keeper)
  )

(defun isStar (v)
  (= v star)
  )

(defun isBoxStar (v)
  (= v boxstar)
  )

(defun isKeeperStar (v)
  (= v keeperstar)
  )

;
; Helper function of getKeeperPosition
;
(defun getKeeperColumn (r col)
  (cond ((null r) nil)
	(t (if (or (isKeeper (car r)) (isKeeperStar (car r)))
	       col
	     (getKeeperColumn (cdr r) (+ col 1))
	     );end if
	   );end t
	);end cond
  )

;
; getKeeperPosition (s firstRow)
; Returns a list indicating the position of the keeper (c r).
; 
; Assumes that the keeper is in row >= firstRow.
; The top row is the zeroth row.
; The first (right) column is the zeroth column.
;
(defun getKeeperPosition (s row)
  (cond ((null s) nil)
	(t (let ((x (getKeeperColumn (car s) 0)))
	     (if x
		 ;keeper is in this row
		 (list x row)
		 ;otherwise move on
		 (getKeeperPosition (cdr s) (+ row 1))
		 );end if
	       );end let
	 );end t
	);end cond
  );end defun

;
; cleanUpList (l)
; returns l with any NIL element removed.
; For example, if l is '(1 2 NIL 3 NIL), returns '(1 2 3).
;
(defun cleanUpList (L)
  (cond ((null L) nil)
	(t (let ((cur (car L))
		 (res (cleanUpList (cdr L)))
		 )
	     (if cur 
		 (cons cur res)
		  res
		 )
	     );end let
	   );end t
	);end cond
  );end 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MY GOAL-STATE HELPER FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; test-row takes in a row (list of integers) and returns T if a box (2) has been found 
;;; and NIL if we reach the end of the list, meaning a box has not been found.
;;;
;;; Basic idea: We test this row if there are any boxes not on a goal; terminate as soon 
;;; as we find at least one box.
(defun test-row (row) 
	(cond 
		((null row) nil)
		((= (car row) box) t)
		(t (test-row (cdr row)))
	);;; end cond
);;; end defun

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; END OF MY GOAL-STATE HELPER FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; EXERCISE: Modify this function to return true (t)
; if and only if s is a goal state of a Sokoban game.
; (no box is on a non-goal square)
;
; Currently, it always returns NIL. If A* is called with
; this function as the goal testing function, A* will never
; terminate until the whole search space is exhausted.
;
;;; goal-test takes in a state s (list of lists) and returns T if all boxes (2's) are 
;;; on a goal and NIL otherwise.
;;;
;;; Basic idea: We iterate through s and pass each row to test-row. If test-row reports 
;;; that a box was found, then we terminate and return NIL since this means there is a 
;;; box not on a goal. If we reach the end of s, then that means no boxes were found, 
;;; so we have reached a goal state and return T.
(defun goal-test (s)
	(cond 
		((null s) t)
		((test-row (car s)) nil)
		(t (goal-test (cdr s)))
	);;; end cond
);;;end defun

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MY NEXT-STATES HELPER FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; get-nth takes in an integer n and a list S and returns the nth element in S, with 
;;; first element starting from index 0.
;;;
;;; Basic idea: nthcdr returns the rest of a list after the (n-1)th element, so we use 
;;; car to extract the front element, which is the nth element. We also check that n 
;;; is not negative, since nthcdr doesn't like negative numbers. If n is negative, 
;;; that means get-square is trying to get the value of a square beyond the bounds of 
;;; of the gameboard, so get-nth should return NIL, so that get-square returns a wall.
(defun get-nth (n S) 
	(cond 
		((< n 0) nil)
		(t (car (nthcdr n S)))
	);;;end cond
);;;end defun

;;; replace-nth takes in an integer n, an element to be inserted v, and a list S, and 
;;; returns a list where the nth element in S has been replaced with v.
;;;
;;; Basic idea: We first get the front elements in S up to the (n-1)th element; to do 
;;; this, we use butlast and pass in the length of S minus n as the number of elements 
;;; to cut off from the end. We then get the back elements from the (n+1)th element to 
;;; the end of S; to do this, we use nthcdr and pass in n plus 1 as the number of 
;;; elements to leave off from the front. We then append the front and the back with 
;;; the element v in between.
(defun replace-nth (n v S) 
	(let 
		((front (butlast S (- (length S) n)))
		 (back (nthcdr (+ 1 n) S)))
		 	(append front (list v) back)
	);;;end let
);;;end defun

;;; get-next takes in a pair of points P represented as (column row) and a direction D 
;;; that is an atom representing one of the four cardinal directions, and returns a 
;;; pair of points that are the result of moving one unit in direction D from point P.
;;;
;;; Basic idea: We use a cond and depending on the direction D, we return a pair of 
;;; points based on P where 1 is added to the appropriate coordinate.
(defun get-next (P D)
	(cond 
		((equal D 'u) (list (car P) (- (cadr P) 1)))
		((equal D 'd) (list (car P) (+ (cadr P) 1)))
		((equal D 'l) (list (- (car P) 1) (cadr P)))
		(t (list (+ (car P) 1) (cadr P)))
	);;;end cond
);;;end defun

;;; update-keeper takes in a state S and the position of the keeper K, and returns 
;;; a new state where the square at the position of the keeper is set to the integer 
;;; of that square without the keeper i.e. blank (0) if the square was just the keeper 
;;; or star (4) if the square was keeperstar.
;;;
;;; Basic idea: We first get the integer of the square at the position of the keeper. 
;;; If the square is keeper (3), then we set the square to blank and return the state; 
;;; otherwise, if the square is keeperstar (6), then we set the square to star and 
;;; return the state.
(defun update-keeper (S K)
	(let 
		((current (get-square S (cadr K) (car K))))
			(cond 
				((isKeeper current) 
					(set-square S (cadr K) (car K) blank))
				((isKeeperStar current) 
					(set-square S (cadr K) (car K) star))
				(t nil));;;end cond
	);;;end let
);;;end defun

;;; valid-move takes in a state S, a direction D, the position of the keeper K, and 
;;; the position N of the square where the keeper would move if he/she moved in the 
;;; direction D. valid-move returns the state if the keeper can and did move in the 
;;; direction D or NIL if the keeper can't move in the direction D.
;;;
;;; Basic idea: We first get the integer at the square the keeper is going to move 
;;; to. We then check if the square is a goal or a blank; if so, then that means the 
;;; keeper is safe to move, so we call update-keeper to set the old square of the 
;;; keeper. Then take the state returned by update-keeper and set the new square 
;;; that keeper will occupy, whether that is keeperstar or just keeper. If the 
;;; square the keeper is going to move to is a box (or boxstar), then we have to 
;;; check the space after the box to see if this box is moveable; to do this, we call 
;;; valid-box-move to handle the different cases of this situation, but we pass in 
;;; the state where we did commit the move as part of the algorithm. If the position 
;;; the keeper is going to move to is not one of these cases, then that means the next 
;;; square is a wall, so we return NIL to indicate an invalid next state.
(defun valid-move (S D K N) 
	(let 
		((next (get-square S (cadr N) (car N))))
			(cond 
				((isBlank next) 
					(set-square (update-keeper S K) (cadr N) (car N) keeper))
				((isStar next) 
					(set-square (update-keeper S K) (cadr N) (car N) keeperstar))
				((isBox next) 
					(valid-box-move 
						(set-square S (cadr N) (car N) keeper) K (get-next N D)))
				((isBoxStar next) 
					(valid-box-move 
						(set-square S (cadr N) (car N) keeperstar) K (get-next N D)))
				(t nil));;;end cond
	);;;end let
);;;end defun

;;; valid-box-move takes in a state S, the position of the keeper K, and the position 
;;; B of the square behind the box in front of the keeper. valid-box-move returns the 
;;; state if the keeper pushed the box in the direction D specified in valid-move or 
;;; NIL if the space behind the box is occupied by another box (or boxstar) or a wall. 
;;;
;;; Basic idea: We first get the integer at the space behind the box and check if it 
;;; is a goal or a blank. If so, then we perform the move by updating the old position 
;;; of the keeper and then set the square of the space behind to box to the integer of 
;;; that space with the box on top. The old position of the box has already been set 
;;; in state S by valid-move. If the space behind the box is not a goal or a blank, 
;;; then it's occupied by a wall or another box, so we return NIL to indicate an 
;;; invalid next state.
(defun valid-box-move (S K B)
	(let 
		((behind_box (get-square S (cadr B) (car B))))
			(cond 
				((isBlank behind_box) 
					(set-square (update-keeper S K) (cadr B) (car B) box))
				((isStar behind_box) 
					(set-square (update-keeper S K) (cadr B) (car B) boxstar))
				(t nil));;;end cond
	);;;end let
);;;end defun

;;; get-square takes in a state S, a row r, and a column c, and returns an integer 
;;; representing the object that occupies the square at (c r) in the state S.
;;;
;;; Basic idea: To get the object at (c r), we simply get the rth element in S to get 
;;; the list representing the rth row, and then get the cth element in the rth row to 
;;; get the object at (c r). So, we call the get-nth function on S with r as n, and 
;;; then call get-nth on the list returned by that with c as n. If the value returned, 
;;; is a valid integer, we return whatever that integer is; if the value returned is 
;;; NIL, get-square is probably being called on an out-of-bounds (c r), so we return a, 
;;; wall.
(defun get-square (S r c) 
	(let 
		((square (get-nth c (get-nth r S))))
			(cond 
				((equal square nil) wall)
				(t square));;;end cond
	);;;end let
);;;end defun

;;; set-square takes in a state S, a row r, a column c, and an integer v, and returns 
;;; the state where the square that occupies the position (c r) is replaced with the 
;;; value v.
;;;
;;; Basic idea: We first get the rth row in S by calling get-nth and replace the cth 
;;; value in that row with v by calling replace-nth. We then replace the rth row in S 
;;; with the newly formed row by calling replace-nth once more, and return the state 
;;; returned by this call to replace-nth.
(defun set-square (S r c v) 
	(replace-nth r (replace-nth c v (get-nth r S)) S)
);;;end defun

;;; try-move takes in a state S, a direction D, and the position of the keeper K, and 
;;; returns the state that would result from the keeper moving one unit in direction 
;;; D. However, if the keeper is not allowed to move in direction D, try-move returns 
;;; NIL.
;;;
;;; Basic idea: try-move will simply call valid-move with the state, direction, and 
;;; position of the keeper, but also the position of the square one unit from the 
;;; position of the keeper in the specified direction. valid-move will then return 
;;; the desired state or NIL.
(defun try-move (S D K) 
	(valid-move S D K (get-next K D))
);;;end defun

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; END OF MY NEXT-STATES HELPER FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; EXERCISE: Modify this function to return the list of 
; sucessor states of s.
;
; This is the top-level next-states (successor) function.
; Some skeleton code is provided below.
; You may delete them totally, depending on your approach.
; 
; If you want to use it, you will need to set 'result' to be 
; the set of states after moving the keeper in each of the 4 directions.
; A pseudo-code for this is:
; 
; ...
; (result (list (try-move s UP) (try-move s DOWN) (try-move s LEFT) (try-move s RIGHT)))
; ...
; 
; You will need to define the function try-move and decide how to represent UP,DOWN,LEFT,RIGHT.
; Any NIL result returned from try-move can be removed by cleanUpList.
; 
;;; next-states takes in a state s and returns all of the possible states that a keeper 
;;; can reach from his/her position.
;;;
;;; Basic idea: We first get the position of the keeper in s and then call try-move in 
;;; all four directions with that position, and compile the resulting states into a 
;;; list to be returned. cleanUpList is called to remove all NIL results in the final 
;;; list.
(defun next-states (s)
	(let* 
		((pos (getKeeperPosition s 0))
		 (result (list 
		 			(try-move s 'u pos) 
					(try-move s 'r pos) 
					(try-move s 'd pos)
					(try-move s 'l pos))))
    		(cleanUpList result)
	);;;end let*
);;;end defun

; EXERCISE: Modify this function to compute the trivial 
; admissible heuristic.
;
;;; h0 takes in a state s, and returns the constant 0.
;;;
;;; Admissibility: h0 is used to return a trivial admissible heuristic, wherein the 
;;; constant 0 that is returned severely underestimates the true cost of the path 
;;; to the solution, since realistically, the cost of the path to reach the goal 
;;; state from the current state is not 0 if the current state is not the goal state.
;;;
;;; Basic idea: We just return 0.
(defun h0 (s)
	0
);;;end defun

; EXERCISE: Modify this function to compute the 
; number of misplaced boxes in s.
;
;;; h1 takes in a state s, and returns the number of boxes that are not on a goal.
;;;
;;; Admissibility: h1 is admissible because the value returned never overestimates 
;;; the true path cost of reaching the goal state from the current state. We know 
;;; this because this heuristic simply counts the number n of boxes not on a goal 
;;; yet, and will be at most the number of boxes not on a goal. However, reaching 
;;; the goal state from the current state will take n or more steps depending on the 
;;; configuration, as the distances between the keeper and boxes, and the boxes and 
;;; the goals are not taken into account. Furthermore, the true path cost of reaching 
;;; the goal state from the current state will never be less than n, since it will 
;;; take at least n moves to move n boxes not on goals to a unique goal per box, 
;;; hence the heuristic never overestimates the true path cost of reaching the 
;;; goal state and is thereby admissible. 
;;;
;;; Basic idea: To get the number of boxes not on a goal, this is essentially 
;;; counting the number of 2's in s. To do this, we iterate over s, and for each 
;;; row, we use the count function to count the number of boxes (2's) in that row. 
;;; We sum the count of each row and return that value, which should be the number 
;;; of misplaced boxes.
(defun h1 (s)
	(cond 
		((null s) 0)
		(t (+ (count box (car s)) (h1 (cdr s))))
	);;;end cond
);;;end defun

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MY HEURISTIC HELPER FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; prune-nth takes in a list l and an integer n, and returns a list where the nth 
;;; element in l has been removed.
;;;
;;; Basic idea: prune-nth uses the same logic as replace-nth, only that instead of 
;;; inserting a new element in place of the nth element when we append the dissected 
;;; list together, we omit the nth element entirely to remove it from the final list.
(defun prune-nth (l n) 
	(let 
		((front (butlast l (- (length l) n)))
		 (back (nthcdr (+ 1 n) l)))
		 	(append front back)
	);;;end let
);;;end defun

;;; get-dist takes a position b and another position g in the form (c r), and returns 
;;; the Manhattan distance between these two positions.
;;;
;;; Basic idea: Since the Manhattan distance means that path between two points must 
;;; consist of vertical and horizontal sub-paths, we can get the Manhattan distance 
;;; between two points by simply subtracting their row values r and subtracting their 
;;; column values c, taking the absolute value of these values, and returning their sum.
(defun get-dist (b g)
	(+ 
		(abs (- (cadr g) (cadr b)))
		(abs (- (car g) (car b)))
	);;;end +
);;;end defun

;;; get-min-dist takes the position of a box b, the list of goal positions g_list for a 
;;; given state, an index i in g_list, another index saved_i in g_list, and a Manhattan 
;;; distance min_dist. get-min-dist will then return a pair consisting of an index in 
;;; g_list and the minimum distance between the box at position b to a goal in g_list.
;;;
;;; Basic idea: get-min-dist will find the goal that is closest to the box at position 
;;; b and return the index of that goal in g_list and the value that is the Manhattan 
;;; distance between that goal and the box. To do this, we iterate over g_list and 
;;; compute the Manhattan distance between the box and the current goal we are looking 
;;; at in g_list by calling get-dist. We then compare this distance to the value in 
;;; min-dist (initialized to be the distance between the first goal and the box) and 
;;; take the value that is less to be the new min-dist argument to the next recursive 
;;; call to get-min-dist. We also get the index of this goal in g_list through addition 
;;; functions. By the end of the recursion, get-min-dist will have saved and returned 
;;; the minimum Manhattan distance between the box and a goal, and the index of that 
;;; goal in g_list.
(defun get-min-dist (b g_list i saved_i min_dist) 
	(cond 
		((null g_list) (list saved_i min_dist))
		(t 
			(let 
				((current (get-dist b (car g_list))))
					(cond 
						((= min_dist -1) (get-min-dist b (cdr g_list) 1 0 current))
						(t 
							(cond 
								((< current min_dist) (get-min-dist b (cdr g_list) (+ i 1) i current))
								(t (get-min-dist b (cdr g_list) (+ i 1) saved_i min_dist))
							));;;end cond
					);;;end cond
			));;;end let
	);;;end cond
);;;end defun

;;; get-x takes in a list row, an integer r that is the index of row in a state, another 
;;; integer c that is the index of a column in a state, and an integer type, and returns 
;;; a list of all positions (c r) of every element in row that matches type.
;;;
;;; Basic idea: get-x iterates through row and compares each integer to type. If an 
;;; element matches, we construct a pair (c r) of the current r and c and cons it with 
;;; the rest of the positions for elements that match type to produce a list of 
;;; "coordinates." Function is called get-x, because it handles the iteration of columns 
;;; which are essentially the x coordinates if the gameboard were represented in a 
;;; Cartesian coordinate system.
(defun get-x (row r c type)
	(cond 
		((null row) '())
		((= (car row) type) 
			(cons 
				(list c r) 
				(get-x (cdr row) r (+ c 1) type)))
		(t 
			(get-x (cdr row) r (+ c 1) type))
	);;;end cond
);;;end defun

;;; get-coords takes in a state s, the index of a row r, and an integer type, and 
;;; returns a list of positions (c r) for every element that matches type in s.
;;;
;;; Basic idea: get-coords iterates over s to get every row. It then calls get-x 
;;; on each row to get the list of positions for elements that match type in that row. 
;;; get-coords then appends all of the resulting lists to get one large list of 
;;; positions for the elements that match type in s.
(defun get-coords (s r type)
	(cond 
		((null s) '())
		(t 
			(append 
				(get-x (car s) r 0 type) 
				(get-coords (cdr s) (+ r 1) type)))
	);;;end cond
);;;end defun

;;; get-greedy takes in a list of boxes b_list, a list of goals g_list, and an integer 
;;; sum, and returns an integer representing the sum of all minimum Manhattan distances 
;;; between the boxes and goals that have yet to be taken by other boxes.
;;;
;;; Basic idea: Essentially, this function uses a greedy algorithm to compute a rough 
;;; estimate of an unbalanced minimum cost bipartite matching between boxes and goals. 
;;; To do this, we call get-min-dist with a box's position (starting from the first in 
;;; b_list), and also pass in g_list and some dummy integers for the get-min-dist 
;;; function. get-min-dist will return the minimum Manhattan distance between the box 
;;; and a goal in g_list, as well as the index of that goal in g_list. Using the index, 
;;; we can call prune-nth to remove that goal from g_list so that the remaining boxes in 
;;; b_list cannot choose an already taken goal as their minimum Manhattan distance goal. 
;;; get-greedy will iterate over b_list and add the minimum Manhattan distances for each 
;;; box-to-goal pair, until every box has found a matching goal, at which point, the 
;;; final sum is returned.
(defun get-greedy (b_list g_list sum)
	(cond 
		((null b_list) sum)
		(t 
			(let 
				((current (get-min-dist (car b_list) g_list 1 0 -1)))
					(get-greedy 
						(cdr b_list)
						(prune-nth g_list (car current))
						(+ sum (cadr current)))
			));;;end let
	);;;end cond
);;;end defun

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; END OF MY HEURISTIC HELPER FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; EXERCISE: Change the name of this function to h<UID> where
; <UID> is your actual student ID number. Then, modify this 
; function to compute an admissible heuristic value of s. 
; 
; This function will be entered in the competition.
; Objective: make A* solve problems as fast as possible.
; The Lisp 'time' function can be used to measure the 
; running time of a function call.
;
;;; h204995126 takes in a state s, and returns an integer that represents the cost of 
;;; a rough unbalanced minimum cost bipartite matching between boxes and goals in s.
;;;
;;; Admissibility: h204995126 is an admissible heuristic as it never overestimates the 
;;; true path cost of reaching the goal state from the current state. This is because, 
;;; although it is an inaccurate minimum cost bipartite matching, since a box earlier 
;;; in the list may take a goal that another box is closer to, this heuristic still 
;;; underestimates the true path cost, as it neglects the cost of having the keeper 
;;; move from box to box after pushing a box to a goal or any walls that may obstruct. 
;;; the keeper's path. Even for configurations where the true path cost of reaching the 
;;; goal state is equal to the number of unmatched boxes n, this algorithm will return 
;;; at most n.
;;;
;;; Basic idea: This heuristic function first gets the position of the keeper, as well 
;;; as a list of positions for all boxes in s that are not on goals and a list of 
;;; positions for all unoccupied goals. The function then checks if the keeper is 
;;; standing on a goal so that it may be added to the goal list. It then calls 
;;; get-greedy with the two lists so that it may compute the assignment between boxes 
;;; and goals. The value returned by get-greedy is then returned by h204995126 to be 
;;; used as the heuristic.
(defun h204995126 (s)
	(let 
		((k_pos (getKeeperPosition s 0))
		 (b_list (get-coords s 0 box))
		 (g_list (get-coords s 0 star)))
			(cond 
				((isKeeperStar (get-square S (cadr k_pos) (car k_pos))) 
					(get-greedy 
						b_list
						(append g_list (list k_pos))
						0))
				(t 
					(get-greedy 
						b_list
						g_list
						0)));;;end cond
	);;;end let
);;;end defun

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Some predefined problems.
 | Each problem can be visualized by calling (printstate <problem>). For example, (printstate p1).
 | Problems are roughly ordered by their difficulties.
 | For most problems, we also privide 2 additional number per problem:
 |    1) # of nodes expanded by A* using our next-states and h0 heuristic.
 |    2) the depth of the optimal solution.
 | These numbers are located at the comments of the problems. For example, the first problem below 
 | was solved by 80 nodes expansion of A* and its optimal solution depth is 7.
 | 
 | Your implementation may not result in the same number of nodes expanded, but it should probably
 | give something in the same ballpark. As for the solution depth, any admissible heuristic must 
 | make A* return an optimal solution. So, the depths of the optimal solutions provided could be used
 | for checking whether your heuristic is admissible.
 |
 | Warning: some problems toward the end are quite hard and could be impossible to solve without a good heuristic!
 | 
 |#

;(80,7)
(setq p1 '((1 1 1 1 1 1)
	   (1 0 3 0 0 1)
	   (1 0 2 0 0 1)
	   (1 1 0 1 1 1)
	   (1 0 0 0 0 1)
	   (1 0 0 0 4 1)
	   (1 1 1 1 1 1)))

;(110,10)
(setq p2 '((1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 1) 
	   (1 0 0 0 0 0 1) 
	   (1 0 0 2 1 4 1) 
	   (1 3 0 0 1 0 1)
	   (1 1 1 1 1 1 1)))

;(211,12)
(setq p3 '((1 1 1 1 1 1 1 1 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 2 0 3 4 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 1 1 1 1 1 1 1 1)))

;(300,13)
(setq p4 '((1 1 1 1 1 1 1)
	   (0 0 0 0 0 1 4)
	   (0 0 0 0 0 0 0)
	   (0 0 1 1 1 0 0)
	   (0 0 1 0 0 0 0)
	   (0 2 1 0 0 0 0)
	   (0 3 1 0 0 0 0)))

;(551,10)
(setq p5 '((1 1 1 1 1 1)
	   (1 1 0 0 1 1)
	   (1 0 0 0 0 1)
	   (1 4 2 2 4 1)
	   (1 0 0 0 0 1)
	   (1 1 3 1 1 1)
	   (1 1 1 1 1 1)))

;(722,12)
(setq p6 '((1 1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 4 1)
	   (1 0 0 0 2 2 3 1)
	   (1 0 0 1 0 0 4 1)
	   (1 1 1 1 1 1 1 1)))

;(1738,50)
(setq p7 '((1 1 1 1 1 1 1 1 1 1)
	   (0 0 1 1 1 1 0 0 0 3)
	   (0 0 0 0 0 1 0 0 0 0)
	   (0 0 0 0 0 1 0 0 1 0)
	   (0 0 1 0 0 1 0 0 1 0)
	   (0 2 1 0 0 0 0 0 1 0)
	   (0 0 1 0 0 0 0 0 1 4)))

;(1763,22)
(setq p8 '((1 1 1 1 1 1)
	   (1 4 0 0 4 1)
	   (1 0 2 2 0 1)
	   (1 2 0 1 0 1)
	   (1 3 0 0 4 1)
	   (1 1 1 1 1 1)))

;(1806,41)
(setq p9 '((1 1 1 1 1 1 1 1 1) 
	   (1 1 1 0 0 1 1 1 1) 
	   (1 0 0 0 0 0 2 0 1) 
	   (1 0 1 0 0 1 2 0 1) 
	   (1 0 4 0 4 1 3 0 1) 
	   (1 1 1 1 1 1 1 1 1)))

;(10082,51)
(setq p10 '((1 1 1 1 1 0 0)
	    (1 0 0 0 1 1 0)
	    (1 3 2 0 0 1 1)
	    (1 1 0 2 0 0 1)
	    (0 1 1 0 2 0 1)
	    (0 0 1 1 0 0 1)
	    (0 0 0 1 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 1 1)))

;(16517,48)
(setq p11 '((1 1 1 1 1 1 1)
	    (1 4 0 0 0 4 1)
	    (1 0 2 2 1 0 1)
	    (1 0 2 0 1 3 1)
	    (1 1 2 0 1 0 1)
	    (1 4 0 0 4 0 1)
	    (1 1 1 1 1 1 1)))

;(22035,38)
(setq p12 '((0 0 0 0 1 1 1 1 1 0 0 0)
	    (1 1 1 1 1 0 0 0 1 1 1 1)
	    (1 0 0 0 2 0 0 0 0 0 0 1)
	    (1 3 0 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 2 1 1 1 0 0 0 1)
	    (1 0 0 0 0 1 0 1 4 0 4 1)
	    (1 1 1 1 1 1 0 1 1 1 1 1)))

;(26905,28)
(setq p13 '((1 1 1 1 1 1 1 1 1 1)
	    (1 4 0 0 0 0 0 2 0 1)
	    (1 0 2 0 0 0 0 0 4 1)
	    (1 0 3 0 0 0 0 0 2 1)
	    (1 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 0 0 0 0 4 1)
	    (1 1 1 1 1 1 1 1 1 1)))

;(41715,53)
(setq p14 '((0 0 1 0 0 0 0)
	    (0 2 1 4 0 0 0)
	    (0 2 0 4 0 0 0)	   
	    (3 2 1 1 1 0 0)
	    (0 0 1 4 0 0 0)))

;(48695,44)
(setq p15 '((1 1 1 1 1 1 1)
	    (1 0 0 0 0 0 1)
	    (1 0 0 2 2 0 1)
	    (1 0 2 0 2 3 1)
	    (1 4 4 1 1 1 1)
	    (1 4 4 1 0 0 0)
	    (1 1 1 1 0 0 0)
	    ))

;(91344,111)
(setq p16 '((1 1 1 1 1 0 0 0)
	    (1 0 0 0 1 0 0 0)
	    (1 2 1 0 1 1 1 1)
	    (1 4 0 0 0 0 0 1)
	    (1 0 0 5 0 5 0 1)
	    (1 0 5 0 1 0 1 1)
	    (1 1 1 0 3 0 1 0)
	    (0 0 1 1 1 1 1 0)))

;(3301278,76)
(setq p17 '((1 1 1 1 1 1 1 1 1 1)
	    (1 3 0 0 1 0 0 0 4 1)
	    (1 0 2 0 2 0 0 4 4 1)
	    (1 0 2 2 2 1 1 4 4 1)
	    (1 0 0 0 0 1 1 4 4 1)
	    (1 1 1 1 1 1 0 0 0 0)))

;(??,25)
(setq p18 '((0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 4 1 0 0 0 0)
	    (0 0 0 0 1 0 2 0 0 0 0 1 0 0 0 0)	    
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)
	    ))
;(??,21)
(setq p19 '((0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 0 1 0 0 1 0 0 0 0)
	    (0 0 0 0 0 0 3 0 0 0 2 0)
	    (0 0 0 0 1 0 0 1 0 0 0 4)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 2 0 4 1 0 0 0)))

;(??,??)
(setq p20 '((0 0 0 1 1 1 1 0 0)
	    (1 1 1 1 0 0 1 1 0)
	    (1 0 0 0 2 0 0 1 0)
	    (1 0 0 5 5 5 0 1 0)
	    (1 0 0 4 0 4 0 1 1)
	    (1 1 0 5 0 5 0 0 1)
	    (0 1 1 5 5 5 0 0 1)
	    (0 0 1 0 2 0 1 1 1)
	    (0 0 1 0 3 0 1 0 0)
	    (0 0 1 1 1 1 1 0 0)))

;(??,??)
(setq p21 '((0 0 1 1 1 1 1 1 1 0)
	    (1 1 1 0 0 1 1 1 1 0)
	    (1 0 0 2 0 0 0 1 1 0)
	    (1 3 2 0 2 0 0 0 1 0)
	    (1 1 0 2 0 2 0 0 1 0)
	    (0 1 1 0 2 0 2 0 1 0)
	    (0 0 1 1 0 2 0 0 1 0)
	    (0 0 0 1 1 1 1 0 1 0)
	    (0 0 0 0 1 4 1 0 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 0 1 4 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 1 1 1 1 1)))

;(??,??)
(setq p22 '((0 0 0 0 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 1 2 0 0 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 1 1 1 0 0 2 1 1 0 0 0 0 0 0 0 0 0)
	    (0 0 1 0 0 2 0 2 0 1 0 0 0 0 0 0 0 0 0)
	    (1 1 1 0 1 0 1 1 0 1 0 0 0 1 1 1 1 1 1)
	    (1 0 0 0 1 0 1 1 0 1 1 1 1 1 0 0 4 4 1)
	    (1 0 2 0 0 2 0 0 0 0 0 0 0 0 0 0 4 4 1)
	    (1 1 1 1 1 0 1 1 1 0 1 3 1 1 0 0 4 4 1)
	    (0 0 0 0 1 0 0 0 0 0 1 1 1 1 1 1 1 1 1)
	    (0 0 0 0 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Utility functions for printing states and moves.
 | You do not need to understand any of the functions below this point.
 |#

;
; Helper function of prettyMoves
; from s1 --> s2
;
(defun detectDiff (s1 s2)
  (let* ((k1 (getKeeperPosition s1 0))
	 (k2 (getKeeperPosition s2 0))
	 (deltaX (- (car k2) (car k1)))
	 (deltaY (- (cadr k2) (cadr k1)))
	 )
    (cond ((= deltaX 0) (if (> deltaY 0) 'DOWN 'UP))
	  (t (if (> deltaX 0) 'RIGHT 'LEFT))
	  );end cond
    );end let
  );end defun

;
; Translates a list of states into a list of moves.
; Usage: (prettyMoves (a* <problem> #'goal-test #'next-states #'heuristic))
;
(defun prettyMoves (m)
  (cond ((null m) nil)
	((= 1 (length m)) (list 'END))
	(t (cons (detectDiff (car m) (cadr m)) (prettyMoves (cdr m))))
	);end cond
  );

;
; Print the content of the square to stdout.
;
(defun printSquare (s)
  (cond ((= s blank) (format t " "))
	((= s wall) (format t "#"))
	((= s box) (format t "$"))
	((= s keeper) (format t "@"))
	((= s star) (format t "."))
	((= s boxstar) (format t "*"))
	((= s keeperstar) (format t "+"))
	(t (format t "|"))
	);end cond
  )

;
; Print a row
;
(defun printRow (r)
  (dolist (cur r)
    (printSquare cur)    
    )
  );

;
; Print a state
;
(defun printState (s)
  (progn    
    (dolist (cur s)
      (printRow cur)
      (format t "~%")
      )
    );end progn
  )

;
; Print a list of states with delay.
;
(defun printStates (sl delay)
  (dolist (cur sl)
    (printState cur)
    (sleep delay)
    );end dolist
  );end defun
