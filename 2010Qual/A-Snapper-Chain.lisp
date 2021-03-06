;;;; GCJ 2010 Qualification Round,
;;;; A. Snapper Chain
;;;; Aleksandr Vinokurov
;;;; 8 May 2010

;;; See problem text for more

(defun light-bulb-p (n k)
  "Answers if the light bulb is ON or OFF
   for N snappers chained and K finger snaps."
  (let ((2^n (expt 2 n)))
    (format nil "~[ON~;OFF~]"
	    (if (zerop (mod (1+ (mod k 2^n)) 2^n))
		0
	        1))))

(defun run-on-stream (&optional &key (in *standard-input*) (out *standard-output*))
  "Runs LIGHT-BULB-P on each case from IN stream and outputs to OUT stream."
  (loop repeat (read in)
       for case from 1
       do (format out "~&Case #~d: ~a~%"
		  case
		  (light-bulb-p (read in) (read in)))))

;; Interactive program function
(defun run (in-file out-file)
  "Run it from the shell with
   ``alisp -L <fasl> -e '(run #p\"~/in.txt\" #p\"~/out.txt\")' -kill''"
  (with-open-file (in in-file)
    (with-open-file (out out-file :direction :output :if-exists :supersede)
      (run-on-stream :in in :out out))))
