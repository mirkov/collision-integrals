;; Mirko Vukovic
;; Time-stamp: <2011-09-05 21:51:47 omega-r-setup.lisp>
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

;; setup the generic functions so that we can add methods in the
;; methods module.  Also set-up the default calling functions


(defmacro defomega*-ij (i j)
  "Expands into forms that define:
- special variable that holds the default calculation method for omega*-ij
- generic function omega-ij*%
- function omega-ij*

For details, of expansion, see unit test defomega*-ij"
  (let ((si (prin1-to-string i))
	(sj (prin1-to-string j)))
    (let ((gf-name (alexandria:symbolicate "OMEGA*%-" si sj))
	  (default-name (alexandria:symbolicate "*OMEGA*-" si sj "-DEFAULT*"))
	  (f-name (alexandria:symbolicate "OMEGA*-" si sj)))
      `(progn
	 (defvar ,default-name nil
	   ,(format nil "Default method for calculating omega*-~a~a" i j))
	 (defgeneric ,gf-name (model T*)
	   (:documentation
	    ,(format nil "Evaluate the omega-~a~a `model' at reduced-temperature T*
If model is specified as `t', use default model" i j))
	   (:method ((model (eql t)) reduced-temperature)
	     (funcall #',gf-name ,default-name reduced-temperature)))
	 (defun ,f-name (T*)
	   ,(format nil "Evaluate omega-~a~a at T* using the default method" i j)
	   (,gf-name t T*))))))

(define-test defomega*-ij
  (assert-expands
   '(progn
     (defvar *omega*-11-default* nil
       "Default method for calculating omega*-11")
     (defgeneric omega*%-11 (model T*)
      (:documentation "Evaluate the omega-11 `model' at reduced-temperature T*
If model is specified as `t', use default model")
      (:method ((model (eql t)) reduced-temperature)
	(funcall #'omega*%-11 *omega*-11-default* reduced-temperature)))
     (defun omega*-11 (T*)
       "Evaluate omega-11 at T* using the default method"
       (omega*%-11 t T*)))
   (defomega*-ij 1 1 )))

(defomega*-ij 1 1)
(defomega*-ij 1 2)
(defomega*-ij 1 3)
(defomega*-ij 1 4)
(defomega*-ij 1 5)
(defomega*-ij 2 2)

(setf *omega*-11-default* *omega*-table*
      *omega*-12-default* *omega*-table*
      *omega*-13-default* *omega*-table*
      *omega*-14-default* *omega*-table*
      *omega*-15-default* *omega*-table*
      *omega*-22-default* *omega*-table*)