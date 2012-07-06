;; Mirko Vukovic
;; Time-stamp: <2011-09-05 21:29:09 setup-default-methods.lisp>
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

(defparameter *omega-calc-defaults*
  (list (list :omega*-11 *omega*-11-default*)
	(list :omega*-12 *omega*-12-default*)
	(list :omega*-13 *omega*-13-default*)
	(list :omega*-14 *omega*-14-default*)
	(list :omega*-15 *omega*-15-default*)
	(list :omega*-22 *omega*-22-default*))
  "Contains an alist of default calculation methods for each integral.
  The cdr is the special variable holding the default method")


(defun default-omega-calc-method (reduced-collision-integral)
  "Return default calculation method for reduced collision integral"
  (cdr (assoc reduced-collision-integral
	      *omega-calc-defaults*)))

(defun set-default-omega-calc-method (reduced-collision-integral object)
  "Set default calculation method for reduced collision integral"
  (setf (cdr (assoc reduced-collision-integral *omega-calc-defaults*))
	object))

(defsetf default-omega-calc-method set-default-omega-calc-method)
