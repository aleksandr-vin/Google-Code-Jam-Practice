;;;; Google Code Jam -- Qualification Round 2009
;;;; C. Welcome to Code Jam
;;;; http://code.google.com/codejam/contest/dashboard?c=90101#s=p2
;;;; Practicing for GCJ 2010, Aleksandr Vinokurov
;;;; Started reading the task at 23:52, 2010-03-18

(setf *print-length* nil) ; Forcing printer to do not skip the data when debugging

(defun count-phrase-subsequencies (phrase text)
  "Returns a count of the phrase subsequencies found in the text.
   If phrase is nil it returns nil.
   Calls CPS as the recursion implementation."
  (when phrase	; we will go further only if it is something in phrase
    (let ((text (sieve phrase text)))
      (cps phrase (length phrase) text (length text))))) ; text will be checked in CPS

(defun sieve (phrase text)
  "Sieves TEXT with the characters from PHRASE."
  (let ((extraction (remove-if-not
		     #'(lambda (c) (member c phrase)) text)))
    ;;(format t ";; Compression ratio is ~d~%" (/ (length extraction) (length text)))
    extraction))

(defparameter *trace-cps* nil
  "Flag for tracing CPS tail recursion (via CPS-MACRO)")

(defparameter *cps-depth* 0
  "Depth of CPS recursion (for tracing only)")

(defmacro cps-macro (phrase phrase-len text text-len)
  "Macro used to trace the CPS function's tail recursion."
  `(progn
     (when *trace-cps* (format t ";; [~d] (CPS-MACRO~%~a~%~a~%~a~%~a)~%"
			       *cps-depth* ,phrase ,phrase-len ,text ,text-len))
     (incf *cps-depth*)
     (let ((result (cps ,phrase ,phrase-len ,text ,text-len)))
       (decf *cps-depth*)
       result)))

(defun cps (phrase phrase-len text text-len)
  "Returns a count of the phrase subsequencies found in the text.
   Should be called from COUNT-PHRASE-SUBSEQUENCIES."
  (declare (optimize debug 0 speed 3 space 0))
  (if (> phrase-len text-len)
      0
      (let ((fp (first phrase))
	    (ft (first text))
	    (rt (rest text))
	    (rt-len (1- text-len)))
	(if (char/= fp ft)
	    (cps-macro phrase phrase-len rt rt-len)
	    (+ (cps-macro phrase phrase-len rt rt-len)
	       (let ((rp (rest phrase))
		     (rp-len (1- phrase-len)))
		 ;;	      (if (and (char= (first rt) ft) (char/= (first rp) fp))
		 (if rp
		     (cps-macro rp rp-len rt rt-len)
		     (progn (when *trace-cps* (format t "^^^^^^^~%")) 1))))))))

(defun trace-cps (&optional (trig t trigp))
  "Triggers the tracing of the CPS function (and its tail recursion
   with the help of CPS-MACRO.
   Optional parameter may turn on (if T) or off (if NIL) this tracing."
  (if trigp
      (if trig
	  (setf *trace-cps* t)
	  (setf *trace-cps* nil))
      (setf *trace-cps* (not *trace-cps*))) ; trigger *trace-cps*
  (if *trace-cps*
      (trace cps)
      (untrace cps))
  *trace-cps*)

(defun test ()
  (fresh-line)
  (format t ";; Testing COUNT-PHRASE-SUBSEQUENCIES: ~a~%"
	  (let ((phrases
		 (list '(a b c)
		       '(a a a)
		       '(a a a)
		       '(a b c)))
		(texts
		 (list '(a b b c c)
		       '(a a a a)
		       '(a a a)
		       '(a a a b c c)))
		(counts
		 (list 4
		       4
		       1
		       6)))
	    (every #'= counts
		   (mapcar #'count-phrase-subsequencies phrases texts)))))

(time (test))

;;(trace-cps)
;;(count-phrase-subsequencies '(a b c) '(a a a b c c))

(defun run-on-stream (&optional &key (in-stream *standard-input*)
		      (out-stream *standard-output*))
  (loop repeat (read in-stream)
     for case from 1
     do (format out-stream "Case #~a: ~a~%"
		case
		(format-4-last-digits
		 (cps-on-next-string in-stream)))))

(defun format-4-last-digits (num)
  (with-output-to-string (s)
    (format s "~4,'0d" (mod num 10000))
    s))

(defun cps-on-next-string (in-stream)
  (count-phrase-subsequencies
   (concatenate 'list "welcome to code jam")
   (concatenate 'list (read-line in-stream))))

(defun test-streamed ()
(fresh-line)
  (format t ";; Testing RUN-ON-STREAM: ~a~%"
	  (string= "Case #1: 0001
Case #2: 0256
Case #3: 0000
"
		   (with-output-to-string (out-stream)
		     (with-input-from-string (in-stream "3
elcomew elcome to code jam
wweellccoommee to code qps jam
welcome to codejam
")
		       (run-on-stream :in-stream in-stream
				      :out-stream out-stream))))))

(time (test-streamed))

;; Interactive program function
(defun run (in-file out-file)
  "Run it from the shell with
   ``alisp -L <fasl> -e '(run #p\"~/in.txt\" #p\"~/out.txt\")' -kill''"
  (with-open-file (in in-file)
    (with-open-file (out out-file :direction :output :if-exists :supersede)
      (run-on-stream :in-stream in :out-stream out))))


;;; Some extra stuff

(defun time-on-file (file)
  (terpri)
  (format t ";; Measuring time on ~a~%" file)
  (time (run file "/var/tmp/GCJ-C-out.txt")))

;;(time-on-file #p"~/Development/GCJ/2009Qual/C-small-practice.in.txt")
;;(time-on-file #p"~/Development/GCJ/2009Qual/C-x.in.txt")

(defun test-run ()
  (with-open-file (in #p"~/Development/GCJ/2009Qual/C-x.in.txt")
    (run-on-stream :in-stream in)))
;;       #p"~/Development/GCJ/2009Qual/C-small-practice.out.txt"))