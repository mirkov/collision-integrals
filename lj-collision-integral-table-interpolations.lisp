;; Mirko Vukovic
;; Time-stamp: <2012-07-12 09:02:04 lj-collision-integral-table-interpolations.lisp>
;; 
;; Copyright 2011 Mirko Vukovic
;; Distributed under the terms of the GNU General Public License
;; 
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


(in-package :omega-xx)

(export '(+t*-min+ +t*-max+))


(defparameter *data-directory*
  (asdf:system-source-directory :collision-integrals)
  "Directory with data files")
;; "Ivchenko-F1a.dat"
(defun read-omega*-table (file-name)
  "Read a table of collision integrals"
  (let ((file (probe-file
	       (merge-pathnames file-name
				*data-directory*)))
	(csv-parser:*field-separator* #\Space))
    (with-input-from-file (stream file)
      (mv-grid:read-grid '(nil 7) stream :csv :eof-value :eof
			 :type 'double-float))))

(defconstant +t*-min+ 0.3d0 "Minimum tabulated value of T*")
(defconstant +t*-max+ 400.0d0 "Maximum tabulatted value of T*")


;;(grid:make-grid '((array (nil 2)) (double-float)) :initial-contents '((1d0 2d0) (3d0 4d0)))

(defclass omega*-table (tabular-data)
  ((column-list :reader column-alist
		:initform nil
		:initarg :column-alist
		:documentation "Alist between the column tables and
		the l,j coefficients"))
  (:documentation "Table of Omega*l-j collision integrals stored as a 2D-array"))

(defmethod print-object ((self omega*-table) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream "Tabulated Omega* integrals")))


(defmethod print-object ((self 2exponent-omega*-fit) stream)
  (print-unreadable-object (self stream :type t :identity t)))

(defmethod describe-object ((self omega*-table) stream)
  (with-slots (doc) self
    (format stream doc)))

(defun make-omega*-table (table-id column-alist)
  "Return an object that contains the tabulated collision integrals
from table specified by TABLE-ID (one of \"F1a\" or \"F1b\").  The
object provides interpolation methods.

COLUMN-ALIST is a list contains a dictionary of Omega indices and table columns.  It is of the format
 (((X0 Y0) . Z0)
  ((X1 Y1) . Z1)
  ...)

Here X? and Y? are the indices of the Omega integral and Z? is the column index.  In these tables
Z? is between 1 and 6.  Column 0 contains T*"
  (let* ((mv-grid::*array-type* 'grid::foreign-array)
	 (file (format nil "Ivchenko-~a.dat" table-id))
	 (table (make-instance 'omega*-table :data
			       (read-omega*-table file)
			       :data-source file
			       :data-documentation 
			       (format nil
"Tabulated Omega* integrals from Ivechenko, Loyalka, et al.
This object contains Omega integrals from table ~a.
Column ~8tOmega (i j)
~{~{~4t~a:~10t~a~}~%~}" table-id
 (mapcar (lambda (entry)
	   ;; we reorder from ((X Y) . Z) into (Z (X Y))
	   (list (cdr entry) (car entry))) column-alist))
			       :column-alist column-alist)))
    (init-table-interp table)
    table))

(defparameter *omega*-table-1* (make-omega*-table "F1a"
					    '(((1 1) . 1)
					      ((1 2) . 2)
					      ((1 3) . 3)
					      ((1 4) . 4)
					      ((1 5) . 5)
					      ((2 2) . 6)))
  "Tabulated omega* values for
indices (1,1), (1,2), (1,3), (1,4), (1,5), and (2,2).  An object of
`tabular-data' type that offers interpolation capabilities")

(defparameter *omega*-table-2* (make-omega*-table "F1b"
					    '(((2 3) . 1)
					      ((2 4) . 2)
					      ((2 5) . 3)
					      ((2 6) . 4)
					      ((3 3) . 5)
					      ((4 4) . 6)))
  "Tabulated omega* values for
indices (2,3), (2,4), (2,5), (2,6), (3,3), (4,4) .  An object of
`tabular-data' type that offers interpolation capabilities")


(defmacro with-verbose-table-interpolation (&body body)
  "Evaluate BODY in a HANDLER-CASE environment that captures T* out of
range"
  `(handler-case
    ,@body
     (gsll:input-domain ()
       (error "T*: ~a is out of range ~a -- ~a" t*
	      +t*-min+ +t*-max+))))

(defmethod omega*%-11 ((table (eql *omega*-table-1*)) t*)
  (with-verbose-table-interpolation
      (interp-table table t* 1)))

(defmethod omega*%-12 ((table (eql *omega*-table-1*)) t*)
  (with-verbose-table-interpolation
    (interp-table table t* 2)))

(defmethod omega*%-13 ((table (eql *omega*-table-1*)) t*)
  (with-verbose-table-interpolation
    (interp-table table t* 3)))

(defmethod omega*%-14 ((table (eql *omega*-table-1*)) t*)
  (with-verbose-table-interpolation
    (interp-table table t* 4)))

(defmethod omega*%-15 ((table (eql *omega*-table-1*)) t*)
  (with-verbose-table-interpolation
    (interp-table table t* 5)))

(defmethod omega*%-22 ((table (eql *omega*-table-1*)) t*)
  (with-verbose-table-interpolation
    (interp-table table t* 6)))

(defmethod omega*%-23 ((table (eql *omega*-table-2*)) t*)
  (with-verbose-table-interpolation
    (interp-table table t* 1)))

(defmethod omega*%-24 ((table (eql *omega*-table-2*)) t*)
  (with-verbose-table-interpolation
    (interp-table table t* 2)))

(defmethod omega*%-25 ((table (eql *omega*-table-2*)) t*)
  (with-verbose-table-interpolation
    (interp-table table t* 3)))

(defmethod omega*%-26 ((table (eql *omega*-table-2*)) t*)
  (with-verbose-table-interpolation
    (interp-table table t* 4)))

(defmethod omega*%-33 ((table (eql *omega*-table-2*)) t*)
  (with-verbose-table-interpolation
    (interp-table table t* 5)))

(defmethod omega*%-44 ((table (eql *omega*-table-2*)) t*)
  (with-verbose-table-interpolation
    (interp-table table t* 6)))



