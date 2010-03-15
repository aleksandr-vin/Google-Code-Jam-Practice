;;;; Google Code Jam -- Qualification Round 2009
;;;; A. Alien Language
;;;; Practicing for GCJ 2010, Aleksandr Vinokurov, 2010-03-07

(defparameter *trace* nil "Tracing of the interpret-test-cases")
(defparameter *l* nil "Word length")
(defparameter *d* nil "Dictionary size")
(defparameter *n* nil "Number of test cases")
(defparameter *dictionary* nil "A dictionary")

(defun read-parameters (&optional stream)
  (setf *l* (read stream))
  (setf *d* (read stream))
  (setf *n* (read stream))
  (list *l* *d* *n*))

(defun read-dictionary (&optional stream)
  (setf *dictionary* nil)
  (dotimes (d *d* *dictionary*)
    (push
     (reverse
      (let ((word nil))
	(dotimes (l *l* word)
	  (push (read-char stream) word))))
     *dictionary*)
    (read-char stream)))		; throwing #\Newline away

(defun interpret-test-cases (&optional stream-in (stream-out t))
  (dotimes (n *n*)
    (format stream-out "Case #~a: " (1+ n))
    (format stream-out "~a~%"
	    (do ((ch (read-char stream-in) (read-char stream-in))
		 ;; A list of tails of matched heads of the words.
		 ;; As we will work only with list, so no copy-tree.
		 ;; All words match a void pattern at startup.
		 (matches (copy-list *dictionary*)))
		((char= ch #\Newline) (length matches))
	      (if (char= ch #\()
		  ;; interpreting list of chars
		  (let ((heads ; collecting a set of heads
			 (do ((ch (read-char stream-in) (read-char stream-in))
			      (heads nil))
			     ((char= ch #\)) heads)
			   (push ch heads))))
		    (setf matches ; saving only
			  (map 'list #'rest ; rest of the tails
			       (remove-if-not ; with matched heads
				#'(lambda (x) (find (first x) heads))
				matches)))
		    (if *trace* (format t "<~a:~a>" heads matches)))
		  ;; interpreting a single character
		  (let ()
		    (setf matches	      ; saving only
			  (map 'list #'rest   ; rest of the tails
			       (remove-if-not ; with matched heads
				#'(lambda (x) (char= (first x) ch))
				matches)))
		    (if *trace* (format t "<~a:~a>" ch matches))))))))

(defun run (&optional filename (stream-out t))
  (with-open-file (stream filename)
    (read-parameters stream)
    (read-dictionary stream)
    (interpret-test-cases stream stream-out)))

(defun run2 (&key filename-in filename-out)
  (with-open-file (stream filename-out :direction :output :if-exists :supersede)
    (run filename-in stream)))

(defun test ()
  (run #p"~/Development/GCJ/2009Qual/Alien language.sample-in"))