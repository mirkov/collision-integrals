;; Mirko Vukovic
;; Time-stamp: <2012-07-11 17:33:28 collision-integrals-user-package-def.lisp>
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

(defpackage :collision-integrals-user
  (:nicknames :ciu)
  (:use :cl :collision-integrals :molecular-potentials :elements :lisp-unit :mv-gnuplot
	:mv-grid)
  (:documentation "Template package for using collision integrals and testing the user interface"))

(in-package :collision-integrals-user)
(define-symbol-macro +kb+ physics-constants:+boltzmann-constant-sp+)
(defconstant +pi+ (coerce pi 'single-float))
(define-symbol-macro +amu+ physics-constants:+atomic-mass-unit-sp+)
