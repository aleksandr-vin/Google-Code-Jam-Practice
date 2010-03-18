;;;; Google Code Jam -- Qualification Round 2009
;;;; C. Welcome to Code Jam
;;;; http://code.google.com/codejam/contest/dashboard?c=90101#s=p2
;;;; Practicing for GCJ 2010, Aleksandr Vinokurov
;;;; Started reading the task at 23:52, 2010-03-18

(defparameter *cps-counter* nil
  "Counter of phrases found in a text by one call of the
   COUNT-PHRASE-SUBSEQUENCIES.")

(defun count-phrase-subsequencies (phrase text)
  "Returns a count of the phrase subsequencies found in the text.
   If phrase is nil it returns nil.
   Calls CPS as the recursion implementation."
  (let ((*cps-counter* 0))
    (when phrase ; we will go further only if it is something in phrase
      (cps phrase text))))		; text will be checked in CPS

(defparameter *trace-cps* nil
  "Flag for tracing CPS tail recursion (via CPS-MACRO)")

(defmacro cps-macro (phrase text)
  "Macro used to trace the CPS function's tail recursion."
  `(progn
     (when *trace-cps* (format t ";; (CPS-MACRO ~a ~a)~%" ,phrase ,text))
     (cps ,phrase ,text)))

(defun cps (phrase text)
  "Returns a count of the phrase subsequencies found in the text.
   Should be called from COUNT-PHRASE-SUBSEQUENCIES."
  (let ((fp (first phrase))
	(ft (first text)))
    (if ft			       ; continue only if we have text
	;; p is not checked here because it is checked in COUNT-PHRASE-SUBSEQUENCIES
	;; and later in CPS
	(let ((rt (rest text)))
	  (cond ((char/= fp ft) (cps-macro phrase rt))
		((char=  fp ft)
		 (let ((rp (rest phrase)))
		   (if rp (cps-macro rp rt)
		       (progn
			 (incf *cps-counter*)
			 (when *trace-cps* (format t "^^^^^^^~%")))))
		 (cps-macro phrase rt))))
	*cps-counter*)))

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
  (format t ";; Testing COUNT-PHRASE-SUBSEQUENCIES: ~a~%"
	  (let ((phrases
		 (list '(a b c)
		       '(a a a)
		       '(a a a)))
		(texts
		 (list '(a b b c c)
		       '(a a a a)
		       '(a a a)))
		(counts
		 (list 4
		       4
		       1)))
	    (every #'= counts
		   (map 'list #'count-phrase-subsequencies phrases texts)))))

(test)

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
    (format s "~a~a~a~a"
	    (mod (floor (/ num 1000)) 10)
	    (mod (floor (/ num 100))  10)
	    (mod (floor (/ num 10))   10)
	    (mod num 10))
    s))

(defmacro string-list (str)
  "String to list macro."
  `(list ,@(with-input-from-string
	   (s str)
	   (loop for char = (read-char s nil nil)
	      while char
	      collect char))))

(defun cps-on-next-string (in-stream)
  (count-phrase-subsequencies
   (string-list "welcome to code jam")
   (loop for char = (read-char in-stream nil nil)
      while char
      until (char= char #\NewLine)
      collect char)))

(defun test-streamed ()
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

(test-streamed)

;; Interactive program function
(defun run (in-file out-file)
  "Run it from the shell with
   ``alisp -L <fasl> -e '(run #p\"~/in.txt\" #p\"~/out.txt\")' -kill''"
  (with-open-file (in in-file)
    (with-open-file (out out-file :direction :output :if-exists :supersede)
      (run-on-stream :in-stream in :out-stream out))))