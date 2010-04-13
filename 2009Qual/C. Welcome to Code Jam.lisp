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
  (if (> phrase-len text-len)
      0
      (let ((fp (first phrase))
	    (ft (first text))
	    (rt (rest text))
	    (rt-len (1- text-len)))
	(if (char/= fp ft)
	    ;; Continue searching the PHRASE in the rest of the TEXT.
	    (cps-macro phrase phrase-len rt rt-len)
	    ;; Or operate with the sub-PHRASE and the rest of the TEXT.
	    (let* ((rp (rest phrase))
		   (rp-len (1- phrase-len))
		   (spsc	     ; Sub-PHRASE subsequencies count.
		    (if rp
			(cps-macro rp rp-len rt rt-len)
			(progn (when *trace-cps* (format t "^^^^^^^~%")) 1))))
	      ;; Note that the code below would not work in case of MOD of the counter!
	      (+ spsc (if (zerop spsc)	; If there are no sub-PHRASE-s
			  0 ; then we can't find whole PHRASE there too.
			  (if
			   ;; Enhancement of the situation when the next char of the TEXT
			   ;; is the same as FT while the second char of the PHRASE differs
			   ;; from the FP. Then we can use the just calculated SPSC as this
			   ;; is a duplication of the case. Or we should calculate a new case
			   ;; fairfuly.
			   (and (char= (first rt) ft) (char/= (first rp) fp))
			   ;;(progn (format t ";; !! Enhancement !! ~a~%" spsc) spsc)
			   spsc
			   (cps-macro phrase phrase-len rt rt-len)))))))))

(defun cps-cmp (fp rp rp-len text rt-len)
  (if (char= fp (first text))
      (if rp
	  (cps-macro rp rp-len (rest text) rt-len)
	  (progn
	    (when *trace-cps* (format t "^^^^^^^~%"))
	    1))
      0))

(defun cps! (phrase phrase-len text text-len)
  "Alternative solution with MAPLIST."
  (if (> phrase-len text-len)
      0
      (let ((fp (first phrase))
	    (rp (rest phrase))
	    (rp-len (1- phrase-len)))
	(reduce #'+
		(maplist #'(lambda (text)
			     (cps-cmp fp rp rp-len text (decf text-len)))
			 text)))))

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
		   (mapcar #'count-phrase-subsequencies phrases texts)))))

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
    (format s "~4,'0d" (mod num 10000))
    s))

(defun doubled-charp (text)
  "Returns NIL if there are no such character X that the TEXT has XX anywhere."
  (notevery #'not
	    (maplist #'(lambda (a) (char= (first a) (first (rest a))))
		     text)))

(defun cps-on-next-string (in-stream)
  (count-phrase-subsequencies
   (concatenate 'list "welcome to code jam")
   (concatenate 'list (read-line in-stream))))

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


;;; Some extra stuff

(defun time-on-file (file)
  (format t ";; Measuring time on ~a~%" file)
  (time (run file "/var/tmp/GCJ-C-out.txt")))

(time-on-file "C-small-practice.in.txt")

(defun test-run ()
  (with-open-file (in #p"~/Development/GCJ/2009Qual/C-x.in.txt")
    (run-on-stream :in-stream in)))
;;       #p"~/Development/GCJ/2009Qual/C-small-practice.out.txt"))