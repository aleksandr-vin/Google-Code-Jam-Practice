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

  