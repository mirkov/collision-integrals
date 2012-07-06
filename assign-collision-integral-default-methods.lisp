;; Mirko Vukovic
;; Time-stamp: <2011-09-04 15:03:57 assign-collision-integral-default-methods.lisp>
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


;; This file deals with the user interface

;; omega-xx% are methods that calculate the reduced integral using a
;; model and temperature.  omega-xx* use the default method.  The
;; latter ones are intended as more user friendly



(setf (default-omega-calc-method :omega*-11) *omega*-table*
      (default-omega-calc-method :omega*-12) *omega*-table*
      (default-omega-calc-method :omega*-13) *omega*-table*
      (default-omega-calc-method :omega*-14) *omega*-table*
      (default-omega-calc-method :omega*-15) *omega*-table*
      (default-omega-calc-method :omega*-22) *omega*-table*)




(define-test default-omega-calc-method
  (assert-equal *omega*-table*
		(default-omega-calc-method :omega*-11))
  (assert-equal *omega*-table* (default-omega-calc-method :omega*-13)))
