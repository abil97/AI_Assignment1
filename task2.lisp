; Variables used to execute part of the code exactly once within a recursive method
(setf check3 t)
(setf check4 t)
(setf check5 t)

; Here idea is to calculate the total cost for the current node and add it as 2nd list element
; i.e. (c 3) --> (c 3 8), where 8 is total cost of 'c' node, starting from the root
(defun Task-2a (l)

		; Put the initial list inside ann empty list
		; And add the total cost to the root node
		(if (not (null check3))
			(progn
				
				; put list inside empty list
				(if (not (null l))  
					(progn	
						; add initial total cost to first node
						(setf first_el (car l))
						(setf first_sum (cadr first_el))
						(setf first_el (add-to-tail first_el first_sum))
						(setf (nth 0 l) first_el)		
					))

				(setf l (list '() l))
				(setf l (cdr l))
				(setf check3 nil)
			)
		)
		(if (not (null l))
			(progn
				(setq next-level nil)			; Initialize empty list
				(dolist (item l ) 				; Going through tree nodes
					(if (atom item) 
						(progn
							(setf item (list '() item))
							(setf item (cdr item)))
					)
					(setq curr (car item))		; get current node
					(write (car curr))			; print its value
					(setq sum_total (caddr curr))	; initially set total cost to the current cost
					(write-char #\,)
					(write sum_total)				; print cost of the node
					(write-char #\Space)

					; Printing the cost of the children
					; Adding child nodes to the list
					(mapcar #'(lambda (x) (and
											  (setq node_to_replace (car x))									; take first node out
											  (setq curr_cost (cadr node_to_replace))							; take its cost
											  (setq sum_final (+ sum_total curr_cost))							; calculating overall node cost
											  (setq node_to_replace (add-to-tail node_to_replace sum_final))	 ; adding overall cost to the node
											  (setf (nth 0 x) node_to_replace)									; adding the updated node to the tree

											  (setf next-level (add-to-tail next-level x))					    ; adding children to the list
							
							))  (cdr item))
					(setq sum_total 0)			; Reset values of total cost 
					(setq sum_final 0)
				)
				(terpri)
				(Task-2a next-level)			; Pass list with children to next recursion level
			)
		)
)

; Helper function that calculates the total cost of each node, but not print it
(defun helper (l)

		; Put the initial list inside ann empty list
		; And add the total cost to the root node
		(if (not (null check4))
			(progn
				; add initial total cost to first node
				(if (not (null l))  
					(progn	
						(setf first_el (car l))
						(setf first_sum (cadr first_el))
						(setf first_el (add-to-tail first_el first_sum))
						(setf (nth 0 l) first_el)		
					))

				; put list inside empty list
				(setf l (list '() l))
				(setf l (cdr l))
				(setf check4 nil)
			)
		)
		(if (not (null l))
			(progn
				(setq next-level nil)
				(dolist (item l ) 
					(if (atom item) 
						(progn
							(setf item (list '() item))
							(setf item (cdr item)))
					)
					(setq curr (car item))				; get current node
					(setq sum_total (caddr curr))		; initially set total cost to the current cost
	
					(mapcar #'(lambda (x) (and
											  (setq node_to_replace (car x))									; take first node out
											  (setq curr_cost (cadr node_to_replace))							; take its cost
											  (setq sum_final (+ sum_total curr_cost))							; calculating overall node cost
											  (setq node_to_replace (add-to-tail node_to_replace sum_final))	; adding overall cost to the node
											  (setf (nth 0 x) node_to_replace)									; adding the updated node to the tree

											(setf next-level (add-to-tail next-level x))						; adding children to the list			
							))  (cdr item))
					(setq sum_total 0)					; reset total cost value
					(setq sum_final 0)
				)
				(helper next-level)						; Pass list with children to next recursion level
			)
		)
)

(defun Task-2b (l)

		(helper l)										

		; Doing the same things as in helper, except there is no need to calculate cost
		; Only printing cost
		(if (not (null check5))
			(progn
				(setf l (list '() l))
				(setf l (cdr l))
				(setf check5 nil)
			)
		)
		(if (not (null l))
			(progn
				(setq next-level nil)
				(dolist (item l ) 

					(if (atom item) 
						(progn
							(setf item (list '() item))
							(setf item (cdr item)))
					)


					(setq curr (car item))			; get current node
					(write (car curr))
					(setq sum_total (caddr curr))	; get its cost
					(write-char #\,)
					(write sum_total)
					(write-char #\;)
					(write-char #\Space)




					(mapcar #'(lambda (x) (and
											  (setq node_to_replace (car x))
											  (write (car node_to_replace))					; print child node value	
											  (write-char #\,)
											  (write (cadr node_to_replace))				; print child node cost
											  (setf next-level (add-to-tail next-level x))	; add children to the list
											  (write-char #\Space)
							
							))  (cdr item))
					(setq sum_total 0)
					(setq sum_final 0)
					(terpri)
				)
				
				(Task-2b next-level)

			)
		)
)

; Reading from input-file and passing to execution
(defun run-all () 
	(let ((in (open "task2-input.txt" :if-does-not-exist nil)))
   		(when in
      		(loop for line = (read-line in nil)  
      			while line do (and 	(set-all-true)
      								(execute-all (car (string-to-list line)))))
      		(close in)
   		)
	)
)


(defun set-all-true ()
	(setf check3 t)
	(setf check4 t)
	(setf check5 t)
)

; convert string to list
(defun string-to-list (str)
        (if (not (streamp str))
           (string-to-list (make-string-input-stream str))
           (if (listen str)
               (cons (read str) (string-to-list str))
               nil)))

; add element to the list
(defun add-to-tail (l x)
   (reverse (cons x (reverse l)))
)


; Execute all functions
(defun execute-all (l)
	(write-line "==========================================================")
	(format t "Executing Task 2 for object:  " #\linefeed)
	(print l)
	(write-line " ")
	(write-line"===========================================================")
	(write-line "TASK 2a: ")
	(write-line " ")
	(Task-2a l)
	(write-line "--------------------------------")
	(write-line "TASK 2b: ")
	(write-line " ")
	(Task-2b l)
	(write-line "--------------------------------")
	(write-line " ")
)


(run-all)
