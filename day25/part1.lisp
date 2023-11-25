(defvar units '("0" "1" "2" "=" "-"))

(defun from-snafu-char (ch)
  (case ch
    ((#\-) -1)
    ((#\=) -2)
    (t (read-from-string (string ch)))))

(defun from-snafu (str)
  (reduce (lambda (acc d) (+ (* 5 acc) (from-snafu-char d)))
	  (concatenate 'list str)
	  :initial-value 0))

(defun to-snafu (num)
  (labels ((f (acc n)
	     (cond
	       ((zerop n) acc)
	       (t (let* ((r (mod n 5))
			 (n1 (+ (truncate (/ n 5)) (if (<= r 2) 0 1))))
		    (f (concatenate 'string (nth r units) acc) n1))))))
    (f nil num)))

(defun solve (input)
  (to-snafu (apply '+ (mapcar #'from-snafu input))))

(solve (with-open-file (lines #P"./input.txt")
	 (loop for l = (read-line lines nil)
	       while l
	       collect l)))
