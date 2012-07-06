;; Mirko Vukovic
;; Time-stamp: <2012-04-25 15:46:07 general-collision-integrals.lisp>
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

!!! Obsolete file.  Should not compile


(in-package :omega-xx)
;; This file calculates the full collision integrals by multiplying
;; the hard-sphere collision integrals by the reduced collision
;; integrals


(defgeneric omega-11 (temperature mass potential-parameters)
  (:documentation "Evaluate the omega-11 integral for a collisions
  characterized with `mass', `temperature' and
  `potential-parameters'.

If potential-parameters is a float, the parameter is intepreted as the
collision diameter, sigma, and the hard sphere integral is calculated.

If it is an object, the approprite method is called that extracts the
potential parameters and calculates the collision integral.
")
  (:method (temperature mass (sigma float))
    "Omega-11 for the hard-sphere potential"
    (omega-11-hs temperature mass sigma))
  (:method (temperature mass (lj-coeffs lj-coeffs))
;;    (print 'hi)
    (* (omega-11-hs temperature mass (coll-sigma lj-coeffs))
	(omega-11* (/ temperature (epsilon/k lj-coeffs))))))

(defgeneric omega-12 (temperature mass potential-parameters)
  (:documentation "Evaluate the omega-12 integral for a collisions
  characterized with `mass', `temperature' and
  `potential-parameters'.

If potential-parameters is a float, the parameter is intepreted as the
collision diameter, sigma, and the hard sphere integral is calculated.

If it is an object, the approprite method is called that extracts the
potential parameters and calculates the collision integral.
")
  (:method (temperature mass (sigma float))
    "Omega-12 for the hard-sphere potential"
    (omega-12-hs temperature mass sigma))
  (:method (temperature mass (lj-coeffs lj-coeffs))
    (* (omega-12-hs temperature mass (sigma lj-coeffs))
	(omega-12* (/ temperature (epsilon/k lj-coeffs))))))

(defgeneric omega-13 (temperature mass potential-parameters)
  (:documentation "Evaluate the omega-13 integral for a collisions
  characterized with `mass', `temperature' and
  `potential-parameters'.

If potential-parameters is a float, the parameter is intepreted as the
collision diameter, sigma, and the hard sphere integral is calculated.

If it is an object, the approprite method is called that extracts the
potential parameters and calculates the collision integral.
")
  (:method (temperature mass (sigma float))
    "Omega-13 for the hard-sphere potential"
    (omega-13-hs temperature mass sigma))
  (:method (temperature mass (lj-coeffs lj-coeffs))
    (* (omega-13-hs temperature mass (sigma lj-coeffs))
	(omega-13* (/ temperature (epsilon/k lj-coeffs))))))

(defgeneric omega-14 (temperature mass potential-parameters)
  (:documentation "Evaluate the omega-14 integral for a collisions
  characterized with `mass', `temperature' and
  `potential-parameters'.

If potential-parameters is a float, the parameter is intepreted as the
collision diameter, sigma, and the hard sphere integral is calculated.

If it is an object, the approprite method is called that extracts the
potential parameters and calculates the collision integral.
")
  (:method (temperature mass (sigma float))
    "Omega-14 for the hard-sphere potential"
    (omega-14-hs temperature mass sigma))
  (:method (temperature mass (lj-coeffs lj-coeffs))
    (* (omega-14-hs temperature mass (sigma lj-coeffs))
	(omega-14* (/ temperature (epsilon/k lj-coeffs))))))

(defgeneric omega-15 (temperature mass potential-parameters)
  (:documentation "Evaluate the omega-15 integral for a collisions
  characterized with `mass', `temperature' and
  `potential-parameters'.

If potential-parameters is a float, the parameter is intepreted as the
collision diameter, sigma, and the hard sphere integral is calculated.

If it is an object, the approprite method is called that extracts the
potential parameters and calculates the collision integral.
")
  (:method (temperature mass (sigma float))
    "Omega-15 for the hard-sphere potential"
    (omega-15-hs temperature mass sigma))
  (:method (temperature mass (lj-coeffs lj-coeffs))
    (* (omega-15-hs temperature mass (sigma lj-coeffs))
	(omega-15* (/ temperature (epsilon/k lj-coeffs))))))

(defgeneric omega-22 (temperature mass potential-parameters)
  (:documentation "Evaluate the omega-22 integral for a collisions
  characterized with `mass', `temperature' and
  `potential-parameters'.

If potential-parameters is a float, the parameter is intepreted as the
collision diameter, sigma, and the hard sphere integral is calculated.

If it is an object, the approprite method is called that extracts the
potential parameters and calculates the collision integral.
")
  (:method (temperature mass (sigma float))
    "Omega-22 for the hard-sphere potential"
    (omega-22-hs temperature mass sigma))
  (:method (temperature mass (lj-coeffs lj-coeffs))
    (* (omega-22-hs temperature mass (sigma lj-coeffs))
	(omega-22* (/ temperature (epsilon/k lj-coeffs))))))



