
; Task 1a
(defun Task-1a (l)
		(if (not (null l))											; Termination condition
			(progn
				(setq next-level nil)								; An empty list, where all child nodes will be placed

				; Going through list elements
				(dolist (item l) 

					; Process atom as a list, if element is atom
					(if (atom item) 
						(progn
							(setf item (list '() item))
							(setf item (cdr item)))
					)
					(write (car item))								;Print current node
					(write-char #\Space)

					; Put all child nodes in the 'next-level' list
					(mapcar #'(lambda (x) (and
											(setf next-level (add-to-tail next-level x))))  (cdr item))
				)
				(terpri)
				(Task-1a next-level)								; Pass list with child nodes to the next recursion level
			)
		)
)

(defun Task-1b (l)
		(if (not (null l))											; Termination condition
			(progn
				(setq next-level nil)								; An empty list, where all child nodes will be placed
				(dolist (item l ) 									; Going through list elements

					; Process atom as a list, if element is atom
					(if (atom item) 
						(progn
							(setf item (list '() item))
							(setf item (cdr item)))
					)
					(write (car item))								; Print current node
					(write-char #\, )

					; Going throung child nodes
					(mapcar #'(lambda (x) (and
											(if (atom x) (write x) (write (car x)))			; Print children
											(write-char #\Space)
											(setf next-level (add-to-tail next-level x))	; Add children to the 'next-level' list					
							)) (cdr item))
					(terpri)
				)
				(Task-1b next-level)								; Pass list to next recursion level
			)
		)
)

; add an element to the end of the list
(defun add-to-tail (l x)
   (reverse (cons x (reverse l)))
)

; executing all functions
(defun execute-all (l)
	(write-line "==========================================================")
	(format t "Executing Task 1 for object:  " #\linefeed)
	(print (car l))
	(write-line " ")
	(write-line "==========================================================")
	(write-line "TASK 1a: ")
	(write-line " ")
	(Task-1a l)
	(write-line "--------------------------------")
	(write-line "TASK 1b: ")
	(write-line " ")
	(Task-1b l)
	(write-line "--------------------------------")
	(write-line " ")
)

; reading input-file contents and passing them to execution
(defun run-all () 
	(let ((in (open "task1-input.txt" :if-does-not-exist nil)))
   		(when in
      		(loop for line = (read-line in nil)
      			while line do (execute-all (string-to-list line)))
      		(close in)
   		)
	)
)


; Convert string to a list
(defun string-to-list (str)
        (if (not (streamp str))
           (string-to-list (make-string-input-stream str))
           (if (listen str)
               (cons (read str) (string-to-list str))
               nil)))
(execute-all '((A (B (C) (D) (E)) (F (G) (H) (I)) (J (K) (L) (M))) ) )