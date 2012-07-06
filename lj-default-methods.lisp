;; Mirko Vukovic
;; Time-stamp: <2011-09-06 20:50:30 lj-default-methods.lisp>
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

(defparameter *omega*-lj-calc-defaults*
  (list (cons :omega*-11 *omega*-11-default*)
	(cons :omega*-12 *omega*-12-default*)
	(cons :omega*-13 *omega*-13-default*)
	(cons :omega*-14 *omega*-14-default*)
	(cons :omega*-15 *omega*-15-default*)
	(cons :omega*-22 *omega*-22-default*))
  "Contains an alist of default calculation methods for each integral.
  The cdr is the special variable holding the default method")


(defun default-omega*-lj-calc-method (omega*)
  "Return default calculation method for reduced collision integral"
  (assert (cdr (assoc omega* *omega*-lj-calc-defaults*))
	  (omega*) "Reduced collision integral ~a not found in *omega*-lj-calc-defaults*"
	  omega*)
  (cdr (assoc omega* *omega*-lj-calc-defaults*)))

(defun set-default-omega*-lj-calc-method (omega* object)
  "Set default calculation method for reduced collision integral"
  (assert (cdr (assoc omega* *omega*-lj-calc-defaults*))
	  (omega*) "Reduced collision integral ~a not found in *omega*-lj-calc-defaults*"
	  omega*)
  (setf (cdr (assoc omega* *omega*-lj-calc-defaults*))
	object))

(defsetf default-omega*-lj-calc-method set-default-omega*-lj-calc-method)
