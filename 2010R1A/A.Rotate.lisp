;;;; GCJ 2010 Round 1A,
;;;; A. Rotate
;;;; Aleksandr Vinokurov
;;;; 22 May 2010

;;; See problem text for more

(defun join-k-in-line (k line)
  (loop for c in line
       and c-1 = () then c
       do (if (char= #\R c c-1) (incf rs)
	      (setf rs (if (char= #\R c) 1 0)))
       do (if (char= #\B c c-1) (incf bs)
	      (setf bs (if (char= #\B c) 1 0)))
       maximize rs into rs-max
       maximize bs into bs-max
       finally (return (list (when (>= rs-max k) :r) (when (>= bs-max k) :b)))))

(defun join-k (n k lines)
  (format t "~&;; n: ~d k: ~d lines: ~a~%" n k lines)
  ;; Join each line
  (labels ((join-in-line (line)
	     (join-k-in-line k line))
	   (join-cols-in-lines (lines)
	     (apply #'mapcar
		    #'(lambda (&rest col)
			(join-in-line col))
		    lines)))
    (let ((jl  (mapcar #'join-in-line lines))
	  (jc  (join-cols-in-lines lines))
	  (jrd (join-cols-in-lines (loop for i from (1- n) downto 0
				      for line in lines
				      collect (append (make-list i)
						      line
						      (make-list (- n i))))))
	  (jld (join-cols-in-lines (loop for i from 0 to (1- n)
				      for line in lines
				      collect (append (make-list i)
						      line
						      (make-list (- n i)))))))
      (format t "~&;; join lines: ~a~%" jl)
      (format t "~&;; join cols: ~a~%" jc)
      (format t "~&;; join r-diags: ~a~%" jrd)
      (format t "~&;; join l-diags: ~a~%" jld)
      (let* ((joins (append jl jc jrd jld))
	     (fR (car (find :r joins :key #'first)))
	     (fB (cadr (find :b joins :key #'second))))
	(if (and fR fB) "Both"
	    (progn (if (and (not fB) (not fR)) "Neither"
		       (if fR "Red" "Blue"))))))))

(defun run-on-stream (&optional &key (in *standard-input*) (out *standard-output*))
  "Runs JOIN-K on each case from IN stream and outputs to OUT stream."
  (loop repeat (read in)
     for case from 1
     do (format out "~&Case #~d: ~a~%"
		case
		(let* ((n (read in))
		       (k (read in))
		       (lines (loop repeat n collect
				   (loop repeat n
				      for c = (read-char in)
				      for cc = (char/= #\. c)
				      when cc
				      collect c into clist
				      counting (not cc) into len
				      finally (return
						(nreverse (append (make-list len)
								  clist))))
				   do (read-char in))))
		  (join-k n k lines)))))

;; Interactive program function
(defun run (in-file out-file)
  "Run it from the shell with
   ``alisp -L <fasl> -e '(run #p\"~/in.txt\" #p\"~/out.txt\")' -kill''"
  (with-open-file (in in-file)
    (with-open-file (out out-file :direction :output :if-exists :supersede)
      (run-on-stream :in in :out out))))
