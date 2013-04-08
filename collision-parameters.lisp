;; Mirko Vukovic
;; Time-stamp: <2013-04-07 22:03:28Eastern Daylight Time collision-parameters.lisp>
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

;; We create collision parameters from molecular potentials using
;; combination rules

(defclass collision-parameters (molecular-potential)
  ((b :initarg :b
      :accessor impact-parameter
      :documentation "Collision impact parameter")
   (v0 :initarg :v0
       :reader v0
       :documentation "Initial velocity")
   (theta :accessor theta
	  :documentation "Deflection angle"))
  (:documentation "Collision parameters.  This includes three types of variables:
- Effective potential parameters, stay fixed
- Parameters that define the collision, can be changed
- Trajectory parameters, are obtained after tracing out a trajectory

The effective potential parameters are:
- mass stores the collision reduced mass
- sigma stores the collision diameter
- epsilon/K stores the collision energy depth

Collision parameters
- b, the impact parameter
- v0, the initial velocity

The trajectory parameters are
- theta, the deflection angle


Units are NOT SI:
- mass in AMU
- sigma in Angstrom"))


(defmethod print-object ((self collision-parameters)
			 stream)
  (print-unreadable-object (self stream :type t :identity t)
    (princ (species self) stream)))

(defmethod describe-object ((self collision-parameters)
			    stream)
  (format stream "Lennard-Jones 6/12 potential coefficients for ~a collision:

The potential parameters are: 
Reduced mass: ~a amu,
Collision diameter: ~a Angstorm,
epsilon/K: ~a~%"
	  (species self) (mass self) (sigma self) (epsilon/K self))
  (format stream "~&The collision parameters are
b: ~a
v0: ~a~%"
	  (if (slot-boundp self 'b) (impact-parameter self) "undefined")
	  (if (slot-boundp self 'V0) (V0 self) "undefined"))
  (format stream "~&The deflection angle is ~a"
	  (if (slot-boundp self 'theta) (theta self) "undefined")))




(defclass lennard-jones-6/12-collision-parameters
    (collision-parameters lennard-jones-6/12-potential)
  ())
(defclass hard-sphere-collision-parameters
    (collision-parameters hard-sphere-potential)
  ())

(defgeneric make-collision-parameters (p1 &optional p2)
  (:documentation
   "Create the collision parameter class for the collision between
potentials `p1' and, optionally `p2'.  `p1' and `p2' must be objects
of type or subtype `molecular-potential'.

If the optional `p2' is provided, the potential is specified by
the combination rules, Kee et al, p. 499

Kee et al do not specify the mass combination rule.  I set it to the
reduced mass of the two species")
  (:method :before ((p1 molecular-potential) &optional p2)
	   (if p2
	       (let ((c1 (class-of p1))
		     (c2 (class-of p2)))
		 (assert (string= (class-name c1)
				  (class-name c2))
			 ()
			 "The two arguments must be objects of same class.  They are
argument1: ~a
argument2: ~a" (class-name c1) (class-name c2)))))
  (:method ((p1 molecular-potential) &optional p2)
    (if p2
	(make-instance 'collision-parameters
		       :species (symbolicate (species p1) "-" (species p2))
		       :mass
		       (let ((m1 (mass p1))
			     (m2 (mass p2)))
			 (/ (* m1 m2)
			    (+ m1 m2)))
		       :sigma
		       (* 0.5d0 (+ (sigma p1) (sigma p2)))
		       :epsilon/k
		       (sqrt (* (epsilon/k p1) (epsilon/k p2))))
	(make-instance 'collision-parameters
		       :species (symbolicate (species p1) "-" (species p1))
		       :mass (* 0.5d0 (mass p1))
		       :sigma (sigma p1)
		       :epsilon/k (epsilon/k p1))))
  (:method ((p1 lennard-jones-6/12-potential) &optional p2)
    (if p2
	(make-instance 'lennard-jones-6/12-collision-parameters
		       :species (symbolicate (species p1) "-" (species p2))
		       :mass
		       (let ((m1 (mass p1))
			     (m2 (mass p2)))
			 (/ (* m1 m2)
			    (+ m1 m2)))
		       :sigma
		       (* 0.5d0 (+ (sigma p1) (sigma p2)))
		       :epsilon/k
		       (sqrt (* (epsilon/k p1) (epsilon/k p2))))
	(make-instance 'lennard-jones-6/12-collision-parameters
		       :species (symbolicate (species p1) "-" (species p1))
		       :mass (* 0.5d0 (mass p1))
		       :sigma (sigma p1)
		       :epsilon/k (epsilon/k p1))))
  (:method ((p1 hard-sphere-potential) &optional p2)
    (if p2
	(make-instance 'hard-sphere-collision-parameters
		       :species (symbolicate (species p1) "-" (species p2))
		       :mass
		       (let ((m1 (mass p1))
			     (m2 (mass p2)))
			 (/ (* m1 m2)
			    (+ m1 m2)))
		       :sigma
		       (* 0.5d0 (+ (sigma p1) (sigma p2)))
		       :epsilon/k 0d0)
	(make-instance 'hard-sphere-collision-parameters
		       :species (symbolicate (species p1) "-" (species p1))
		       :mass (* 0.5d0 (mass p1))
		       :sigma (sigma p1)
		       :epsilon/k (epsilon/k p1)))))






(define-test make-lennard-jones-6/12-collision-parameters
  (let* ((molecular-potentials:*lennard-jones-6/12-coeffs*
	  molecular-potentials:*test-lennard-jones-6/12-coeffs*)
	 (p1 (make-species-lennard-jones-6/12-potential :X))
	 (p2 (make-species-lennard-jones-6/12-potential :Y))
	 (lj-coeffs (make-collision-parameters p1 p2)))
    (describe lj-coeffs)
    (let ((lisp-unit:*epsilon* 1e-6))
      (assert-number-equal (/ 2.0 3.0) (mass lj-coeffs))
      (assert-number-equal 2.5 (sigma lj-coeffs))
      (assert-number-equal (sqrt 20.0) (epsilon/k lj-coeffs)))))

#|
(defgeneric make-collision-parameters (species1 species2)
  (:documentation
"Return an object with collision parameters between species1 and species2

spec"))|#
