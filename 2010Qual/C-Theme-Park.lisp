;;;; GCJ 2010 Qualification Round,
;;;; B. Theme Park
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
  "Computes the amount of Euros made by the roller coaster for each case
   from the IN stream and writes it to the OUT stream."
  (loop repeat (read in)		; T cases => T x 2 lines
     for case from 1
     do (format out "~&Case #~d: ~a~%"
		case
		(roller-coaster :R (read in)
				:k (read in)
				:gs (loop repeat (read in) ; First line ends with N => N integers follow on the second line
				       collect (read in))))))

(defun roller-coaster (&key R k gs)
  (format t "~&;; R: ~d k: ~d gs: ~a~%" R k gs)
  (let ((cache gs)
	(top-R R))
    (labels ((rc (R k gs euros)
	       (if (plusp R)
		   (if (every #'= gs cache)
		       (let ((loopR (- top-R R)))
			 (multiple-value-bind (dR mR) (floor top-R loopR)
			   (format t "~&;; Looped on ~d, dR: ~d, mR: ~d~%;; ~d iterations optimized!!~%"
				   loopR dR mR (* dR loopR))
			   (if (zerop mR)
			       (* dR euros)
			       (let* ((load (load-rc gs k))
				      (earn (car load))
				      (gs (cdr load)))
				 (rc (1- mR) k gs (+ earn (* dR euros)))))))
		       (let* ((load (load-rc gs k))
			      (earn (car load))
			      (gs (cdr load)))
			 (rc (1- R) k gs (+ earn euros))))
		   euros)))
      (let* ((load (load-rc gs k))
	     (earn (car load))
	     (gs (cdr load)))
	(rc (1- R) k gs (+ earn 0))))))

(defun load-rc (gs k)
  "Loads roller coaster and returns cons with # loaded (Euros) at CAR and new
   queue as CDR."
  (do* ((gs gs)				; Taking group from here
	(g (car gs))
	(pass ())			; Putting group here
	(load 0))
       ((or (atom gs) (> (+ load g) k))
	(cons load (append gs (nreverse pass))))
    (setf gs (cdr gs))
    (incf load g)
    (setf pass (cons g pass))
    (setf g (car gs))))
   ;;(format t "~&;; g: ~a gs: ~a pass: ~a load ~a~%" g gs pass load)))

(defun test1 ()
  (format t "~&;; test1: ~[OK~;False!~]~%"
	  (if (every #'(lambda (x) (and x))
		     (with-open-file
			 (result "~/Development/GCJ/2010Qual/C-small-attempt0.out.txt")
		       (with-input-from-string
			   (str (with-output-to-string (out)
				  (with-open-file
				      (in "~/Development/GCJ/2010Qual/C-small-attempt0.in.txt")
				    (run-on-stream :in in :out out))))
			 (loop for r = (read-line result nil)
			    for i = (read-line str nil)
			    while (and r i)
			    collect (string= r i))))) 0 1)))
      
    