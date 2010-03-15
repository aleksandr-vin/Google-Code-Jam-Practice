;;;; Google Code Jam -- Qualification Round 2009
;;;; B. Watersheds
;;;; Practicing for GCJ 2010, Aleksandr Vinokurov, 2010-03-10

(defparameter *trace* nil "Tracing of the program")

(defun trace! (&rest args)
  (when *trace* (apply #'format (cons t args))))

(defclass geo-map ()
  ((height :initarg :height)
   (width  :initarg :width)
   (data   :initarg :data))) ; 2D array of cells: (list elevation drainage)

(defun elevation (cell)
  (car cell))

(defun drain (cell)
  (cdr cell))

(defparameter *sink-counter* 0 "Global counter of sinks")

(defun make-sink (cell)
  "Create a sink cell."
  (cons (elevation cell) (list (incf *sink-counter*))))

(defun link (from-cell to-cell)
  "Connect from-cells' drain-link to to-cells' drain"
  (setf (cadr from-cell)
	(drain to-cell)))

(defun neighboring-cells (data row col)
  (list (aref data (1- row)     col)	; North
	(aref data     row  (1- col))	; West
	(aref data     row  (1+ col))	; East
	(aref data (1+ row)     col)))	; South

(defun drainer (cell neighbours)
  "Return one element from neighbours to whom the cell drains.
   Or a sink if there are no one appropriate for it."
  (let ((lowest (first (stable-sort (remove (elevation cell) neighbours
					    :key #'elevation :test-not #'>)
				    #'< :key #'elevation))))
    (if lowest lowest (make-sink cell))))

(defun connect-drains-on-map (geo-map)
  "Connect cells' drainages by the rules from the task,
   see http://code.google.com/codejam/contest/dashboard?c=90101#s=p1
   Organizing them into directed acyclic graphs with sinks at their
   ends. One DAG for a drainage basin.
   Such DAG has one propery: it translates the sinks' label through
   all its nodes"
  (with-slots (height width data) geo-map
    (loop for row from 1 to height
       do (loop for col from 1 to width
	     do (let ((cell (aref data row col)))
		  (link cell
			(drainer cell
				 (neighboring-cells data row col)))
		  (trace! "; ~a~%" data)))))
  geo-map)

(defun find-last-cons (y)
  (when (consp y)
    (if (not (consp (car y))) y
	(find-last-cons (car y)))))

;; Tests for FIND-LAST-CONS
(format t ";; Testing FIND-LAST-CONST: ~a~%"
	(and
	 (eq    nil   (find-last-cons :a))
	 (equal '(:a) (find-last-cons '(:a)))
	 (equal '(:a) (find-last-cons '(((:a)))))))

(defparameter *last-used-char-code* nil "Counter of the char codes for labelling sinks")

(defun basin-label (drainer)
  "Return label of the basin the drainer is member of.
   If no label is provided for the basin, a new one is generated."
  (flet ((gen-sink-label ()
	   (code-char (incf *last-used-char-code*))))
    (let ((last-cons (find-last-cons drainer)))
      (if (numberp (car last-cons))
	  (setf (car last-cons) (gen-sink-label))
	  (car last-cons)))))

(defun label-map (geo-map)
  "Label the maps' drainage basins with the characters alphabetically
   from left to right, from top to bottom."
  ;; Each map should have sinks labelled from #\a
  (let ((*last-used-char-code* (1- (char-code #\a)))) 
    (with-slots (height width data) geo-map
      (loop for row from 1 to height
	 do (loop for col from 1 to width
	       do (let ((cell (aref data row col)))
		    (setf (cdr cell) (basin-label (drain cell)))
		    (trace! "; ~a~%" data))))))
  geo-map)

(defun to-string (geo-map)
 (with-slots (height width data) geo-map
      (loop for row from 1 to height
	 do (loop for col from 1 to width
	       do (let ((basin (cdr (aref data row col))))
		    (format t "~a " basin))))))

(defun read-next-map (stream)
  "Read a map from the stream.
   Two numbers H and W -- height and width -- are read from the first line.
   Then H lines follow with W numbers on each -- that is a map of cells' elevations.
   Return an instance of geo-map where data is a 2D array of that map surrounded
   with a border of cells with elevations of undoubted hight. Its size is (H+2)x(W+2)."
  (let* ((height (read stream))
	 (width  (read stream))
	 (data   (make-array (list (+ height 2) (+ width 2))
			     :initial-element '(10000))))
    (loop for row from 1 to height
       do (loop for col from 1 to width
	     do (setf (aref data row col)
		      (list (read stream)       ; elevation
			    (list (gensym)))))) ; cons cell for drain-link
    (make-instance 'geo-map
		   :height height
		   :width  width
		   :data   data)))

(defun label-maps (&optional &key (in-stream *standard-input*)
		   (out-stream *standard-output*))
  "Read T -- the number of maps -- from in-stream
   and label next T maps from that stream."
  (loop
     for case from 1 to (read in-stream)
     for map = (read-next-map in-stream)
     do (format out-stream "Case #~a:~%~a~%"
		case (to-string (label-map (connect-drains-on-map map))))))

(with-input-from-string (in-stream
			 "1
3 3
9 6 3
5 9 6
3 5 9")
  (let ((*trace* t))
    (label-maps :in-stream in-stream)))