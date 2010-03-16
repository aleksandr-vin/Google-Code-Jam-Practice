;;;; Google Code Jam -- Qualification Round 2009
;;;; B. Watersheds
;;;; Practicing for GCJ 2010, Aleksandr Vinokurov, 2010-03-10

(defparameter *test*  t "Testing of the program")
(defparameter *trace* nil "Tracing of the program")

(defun trace! (&rest args)
  (when *trace* (apply #'format (cons t args))))

(defun elevation (cell)
  (car cell))

(defun drain (cell)
  (cdr cell))

(defparameter *sink-counter* 0 "Global counter of sinks")

(defun make-sink (cell)
  "Create a sink cell."
  (cons (elevation cell) (list (incf *sink-counter*))))

(defun link (from-cell to-cell)
  "Returns a cell with from-cells' elevation and drain-link pointing
   to to-cells' drain. Modifies from-cell."
  (setf (cadr from-cell)
	(drain to-cell))
  from-cell)

(defun drainer (cell neighbours)
  "Return one element from neighbours to whom the cell drains.
   Or a sink if there are no one appropriate for it."
  (let ((lowest (first (stable-sort (remove (elevation cell) neighbours
					    :key #'elevation
					    :test (complement #'>))
				    #'< :key #'elevation))))
    (if lowest lowest (make-sink cell))))

(defun connect-drains-for-cell (cell north west east south)
  (link cell (drainer cell (list north west east south))))

(defun connect-drains-on-a-parallel (par-1 par par+1)
  (map 'list #'connect-drains-for-cell
       (cdr par)			; Operation cell
       (cdr par-1)			; North neighbour cell
       par				; West neighbour cell
       (cddr par)			; East neighbour cell
       (cdr par+1)))			; South neighbour cell

(defun connect-drains-on-map (geo-map)
  "Modify geo-map in a way that all cells that have 4 neighbours get their drains
   connected to one of the neighbours or a sink. See task on Google Code Jam site
   for details.
   Returns new list of lists of cells that point to the modified original cells.
   Border cells are not included, so each lists' length is twice decremented.
   Such a map will be called unbordered-map."
  (map 'list #'connect-drains-on-a-parallel
       geo-map				; Northen parallel
       (cdr geo-map)			; Operation parallel
       (cddr geo-map)))			; Southen parallel

(defun find-last-cons (y)
  (when (consp y)
    (if (not (consp (car y))) y
	(find-last-cons (car y)))))

;; Tests for FIND-LAST-CONS
(if *test*
    (format t ";; Testing FIND-LAST-CONST: ~a~%"
	    (and
	     (eq    nil   (find-last-cons :a))
	     (equal '(:a) (find-last-cons '(:a)))
	     (equal '(:a) (find-last-cons '(((:a))))))))

(defparameter *last-used-char-code* nil
  "Counter of the char codes for labelling sinks.
   Must be reset to the starting char-code before labelling a map.")

(defun sink-label (last-cons)
  "Returns label of the sink -- the last-cons actually point to the sink --
   or generate a new label, if this sink has no label for now.
   Modifies the sink."
  (flet ((gen-sink-label ()
	   (code-char (incf *last-used-char-code*))))
    (if (numberp (car last-cons))
	(setf (car last-cons) (gen-sink-label))
	(car last-cons))))

(defun basin-label (drainer)
  "Return label of the basin the drainer is member of.
   If no label is provided for the basin, a new one is generated.
   Modifies the drainer."
  (sink-label (find-last-cons drainer)))

(defun cell-basin-label (cell)
  (basin-label (drain cell)))

(defun label-parallel (par)
  (map 'list #'cell-basin-label par))

(defun label-map (unbordered-map)
  "Label the maps' drainage basins with the characters alphabetically
   from left to right, from top to bottom.
   Labels start from #\a.
   Modifies the drainers of the cells.
   Returns a new map (list of lists)."
  (let ((*last-used-char-code* (1- (char-code #\a))))
    (map 'list #'label-parallel unbordered-map)))

(defmacro join-strings-with-char (ch a b)
  `(concatenate 'string (string ,a) ,(string ch) (string ,b)))

(defun join-strings-with-space (a b)
  (join-strings-with-char #\Space a b))

(defun join-strings-with-newline (a b)
  (join-strings-with-char #\NewLine a b))

(defun to-string (unbordered-map)
  (reduce #'join-strings-with-newline
	  (map 'list #'(lambda (x)
			   (reduce #'join-strings-with-space x
				   :from-end t))
	       unbordered-map)
	  :from-end t))


(defun read-next-parallel (stream width heavens)
  (concatenate 'list `(,heavens)
	       (loop repeat width
		  collect (cons (read stream) ; elevation
				(list (gensym)))) ; cons cell for drain-link
	       `(,heavens)))

(defun read-next-map (stream &optional (border-elevation 10000))
  "Read a map from the stream.
   Two numbers H and W -- height and width -- are read from the first line.
   Then H lines follow with W numbers on each -- that is a map of cells' elevations.
   Return a list of lists that represent the map of cells. The first and last lists
   are a North and South borders with the highest+1 elevations allowed. The first
   and last elements of each sublist are West and East borders for the cells on
   a parallel."
  (let* ((height (read stream))
	 (width  (read stream))
	 (heavens `(,border-elevation))
	 (parallel-heavens (make-list (+ width 2) :initial-element heavens)))
    (concatenate 'list `(,parallel-heavens)
		 (loop repeat height
		    collect (read-next-parallel stream width heavens))
		 `(,parallel-heavens))))

(defun label-maps (&optional &key (in-stream *standard-input*)
		   (out-stream *standard-output*))
  "Read T -- the number of maps -- from in-stream
   and label next T maps from that stream."
  (loop
     for case from 1 to (read in-stream)
     for map = (read-next-map in-stream)
     do (format out-stream "Case #~a:~%~a~%"
		case (to-string (label-map (connect-drains-on-map map))))))


;; Program test
(if *test*
    (format t ";;; Program test: ~a~%"
	    (string= "Case #1:
a b b
a a b
a a a
Case #2:
a a a a a a a a a b
Case #3:
a a a
b b b
Case #4:
a a a a a
a a b b a
a b b b a
a b b b a
a a a a a
Case #5:
a b c d e f g h i j k l m
n o p q r s t u v w x y z
"
		     (with-output-to-string (out-stream)
		       (with-input-from-string (in-stream "5
3 3
9 6 3
5 9 6
3 5 9
1 10
0 1 2 3 4 5 6 7 8 7
2 3
7 6 7
7 6 7
5 5
1 2 3 4 5
2 9 3 9 6
3 3 0 8 7
4 9 8 9 8
5 6 7 8 9
2 13
8 8 8 8 8 8 8 8 8 8 8 8 8
8 8 8 8 8 8 8 8 8 8 8 8 8
")
			 (label-maps :in-stream in-stream :out-stream out-stream))
		       out-stream))))

;; Interactive program function
(defun run (in-file out-file)
  "Run it from the shell with
   ``alisp -L <fasl> -e '(run #p\"~/in.txt\" #p\"~/out.txt\")' -kill''"
  (with-open-file (in in-file)
    (with-open-file (out out-file :direction :output :if-exists :supersede)
      (label-maps :in-stream in :out-stream out))))
