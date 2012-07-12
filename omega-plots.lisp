(in-package :collision-integrals-user)

(defun plot-table (title table)
  (let* ((legend (omega-xx::column-alist table))
	 (columns (length legend))
	 (ys
	  (loop for column from 1 to columns
	     for legend-entry in legend
	     collecting (list
			 (mv-grid+gsll::table-column table column)
			 :title (car legend-entry)))))
    (set-to ((logscale :x)
	     (xlabel "T*")
	     (ylabel "Omega")
	     (title title))
      (plot-xys (mv-grid+gsll::table-column table 0)
		ys))))


(define-plot F1a-table-plot
  (plot-table "Omega integrals in table F1a" omega-xx::*omega*-table-1*))

(define-plot F1b-table-plot
  (plot-table "Omega integrals in table F1b" omega-xx::*omega*-table-2*))


(defparameter *t*-vec* (gseq (* 1.0001 +t*-min+) (/ +t*-max+ 1.0001))
  "Geometric sequence of T* values between 0.3 and 400")

(defun plot-interpolations ()
  "Plot all tabulated Omega functions, interpolated on a sequence of "
  (let* ((indices
	  (append
	   (mapcar #'car (omega-xx::column-alist omega-xx::*omega*-table-1*))
	   (mapcar #'car (omega-xx::column-alist omega-xx::*omega*-table-2*))))
	 (omega-funs (mapcar (lambda (indices)
			      (intern
			       (format nil "OMEGA*-~{~a~a~}" indices)))
			    indices))
	 (ys
	  (loop for omega-fun in omega-funs
	       for index in indices
	       collecting (list
			   (gmap (symbol-function omega-fun) *t*-vec*)
			   :title index))))
    (set-to ((logscale :x)
	     (xlabel "T*")
	     (ylabel "Omega")
	     (title "Omega, interpolated on a geometric sequence of T^*"))
    (plot-xys *t*-vec* ys))))
