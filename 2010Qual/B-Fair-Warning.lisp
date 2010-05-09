;;;; GCJ 2010 Qualification Round,
;;;; B. Fair Warning
;;;; Aleksandr Vinokurov
;;;; 8 May 2010

;;; See problem text for more

;; Interactive program function
(defun run (in-file out-file)
  "Run it from the shell with
   ``alisp -L <fasl> -e '(run #p\"~/in.txt\" #p\"~/out.txt\")' -kill''"
  (with-open-file (in in-file)
    (with-open-file (out out-file :direction :output :if-exists :supersede)
      (run-on-stream :in in :out out))))

(defun run-on-stream (&optional &key (in *standard-input*) (out *standard-output*))
  "Computes the amount of time until the apocalypse comes for each case
   from the IN stream and writes it to the OUT stream."
  (loop repeat (read in) ; C lines => C cases
       for case from 1
       do (format out "~&Case #~d: ~a~%"
		  case
		  (time-to-apocalypse (loop repeat (read in) ; Line starts with N => N integers follow
		     collect (read in))))))

(defun time-to-apocalypse (times)
  "Computes the amount of time until the apocalypse comes
   for Great Events that occured in TIMES."
  (format t "~&;; ~a" times)
  (flet ((t-j (t-k-j-1 a-j-1 gcd-j-1)
	   (1+ (/ (- t-k-j-1 a-j-1) gcd-j-1))))
    (if (member 1 times)
	0
	(let ((gcd (apply #'gcd times)))
	  (format t " gcd: ~a" gcd)
	  (if (> gcd 1)
	      (* gcd (time-to-apocalypse (mapcar #'(lambda (x) (/ x gcd)) times)))
	      (let* ((ac (find-min-alpha-with-max-gcd times))
		     (a (car ac))
		     (a-gcd (cdr ac)))
		(if (plusp a)
		    (- (* a-gcd
			  (1+ (time-to-apocalypse
			   (mapcar #'(lambda (x) (t-j x a a-gcd)) times))))
		       a)
		    0)))))))

(defun find-alpha (xs alpha)
  (if (member alpha xs)
      (cons 0 1)
      (let ((gcd (apply #'gcd (mapcar #'(lambda (x) (- x alpha)) xs))))
	(if (> gcd 1)
	    (cons alpha gcd)
	    (find-alpha xs (1+ alpha))))))

(defun find-min-alpha-with-max-gcd (xs)
  (let ((min (apply #'min xs)))
    (flet ((min-ac (ac1 ac2)
	     "Minimum alpha and maximum GDC"
	     (if (> (car ac1) (car ac2))
		 (if (plusp (car ac2)) ; Skipping zeroed alphas
		     ac2
		     ac1)
		 (if (< (car ac1) (car ac2))
		     ac1
		     (if (> (cdr ac1) (cdr ac2))
			 ac1
			 ac2)))))
      (do* ((ac (find-alpha xs 0) (find-alpha xs i))
	    (i (car ac) (+ i (if (plusp (car ac)) (car ac) 1)))
	    (min-ac ac (min-ac min-ac ac))
	    (j 0 (1+ j)))
	   ((or (>= i min) (zerop (car ac))) min-ac)))))
     ;; (format t "~&;; min: ~d ac: ~a i: ~d j: ~d~%" min ac i j))))