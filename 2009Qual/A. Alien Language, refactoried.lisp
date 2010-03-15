;;;; Google Code Jam -- Qualification Round 2009
;;;; A. Alien Language
;;;; Practicing for GCJ 2010, Aleksandr Vinokurov, 2010-03-07
;;;;
;;;; Refactoried code.

(defparameter *trace*      nil "Tracing of the interpret-test-cases")
(defparameter *in*         nil "Input stream for the program")
(defparameter *out*        t   "Output stream for the program")
(defparameter *l*          nil "Word length")
(defparameter *d*          nil "Dictionary size")
(defparameter *n*          nil "Number of test cases")
(defparameter *dictionary* nil "A dictionary")

(defun read-parameters ()
  (setf *l* (read *in*))
  (setf *d* (read *in*))
  (setf *n* (read *in*))
  (list *l* *d* *n*))

(defun read-dictionary ()
  (setf *dictionary* nil)
  (dotimes (d *d* *dictionary*)
    (push
     (reverse
      (let ((word nil))
	(dotimes (l *l* word)
	  (push (read-char *in*) word))))
     *dictionary*)
    (read-char *in*)))		; throwing #\Newline away

(defmacro map-rest-of-remove-if-not (testp list)
  `(setf ,list
	 (map 'list #'rest
	      (remove-if-not ,testp ,list))))

(defun make-head-pattern (from-char)
  (if (char= from-char #\()
      ;; interpreting list of chars
      (let ((heads			; collecting a set of heads
	     (do ((ch (read-char *in*) (read-char *in*))
		  (heads nil))
		 ((char= ch #\)) heads)
	       (push ch heads))))
	(if *trace* (format t "<~a:" heads))
	#'(lambda (x) (find (first x) heads)))
      ;; interpreting a single character
      (let ()
	(if *trace* (format t "<~a:" from-char))
	#'(lambda (x) (char= (first x) from-char)))))

(defun interpret-test-cases ()
  "Solves the problem for each test case, see the task
   at http://code.google.com/codejam/contest/dashboard?c=90101#s=p0"
  (dotimes (n *n*)
    (format *out* "Case #~a: " (1+ n))
    (format *out* "~a~%"
	    (do ((ch (read-char *in*) (read-char *in*))
		 ;; A list of tails of matched heads of the words.
		 ;; As we will work only with list, so no copy-tree.
		 ;; All words match a void pattern at startup.
		 (matches (copy-list *dictionary*)))
		((char= ch #\Newline) (length matches))
	      (map-rest-of-remove-if-not (make-head-pattern ch) matches)
	      (if *trace* (format t "~a>" matches))))))

(defmacro run (&optional &key (input-file nil) (output-file nil))
  "Set up input and output files (if supplied)
   reads parameters, dictionary and interpret
   test cases."
  (let ((code
	 '(progn
	   (read-parameters)
	   (read-dictionary)
	   (interpret-test-cases))))
    (when input-file
      (setf code
	    (nconc `(with-open-file (*in* ,input-file))
		   (list code))))
    (when output-file
      (setf code
	    (nconc `(with-open-file (*out* ,output-file :direction :output
					   :if-exists :supersede))
		   (list code))))
    code))

(defun test ()
  (run :input-file #p"~/Development/GCJ/2009Qual/Alien language.sample-in"))

;(test)