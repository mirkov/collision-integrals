;; Mirko Vukovic
;; Time-stamp: <2011-10-17 10:01:14 omega-hs.lisp>
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

(define-symbol-macro +kb+ +boltzmann-constant-sp+)
(define-symbol-macro +amu+ physics-constants:+atomic-mass-unit-sp+)
(defconstant +pi+ (coerce pi 'single-float))
(defconstant +omega-hs-constant+
  (* (sqrt (/ (* +pi+ +kb+)
	      (* 2.d0 +amu+)))
     (^2 1d-10))
  "Constant that figures in the hard-sphere collision integral 
(Ferziger & Peric, 7.1-28)")

;; we start out with the general scaling for hard-sphere collisions
;; and the formula for the 11 collision integrals.  These two are used
;; in a macro that follows
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun omega-LS-hs* (l s)
    "Ratio of the general hard-sphere collision integral to the 11
  hard-sphere omega

l and s are limited to below 6, for one reason to limit overflows in
the factorial calculation of (1+s)!

Kee et al, (12.95)"
    (assert (< s 6) () "Maximum value of s is 5")
    (assert (< l 6) () "Maximum value of l is 5")
    (let ((s+1! (loop for i from 1 to (1+ s)
		   with fact = 1
		   do (setf fact (* fact i))
		   finally (return fact))))
      (* (/ s+1!
	    2)
	 (- 1 (* 1/2 (/ (+ 1 (expt -1 l))
			(+ 1 l)))))))

  (defun Omega-11-hs1 (m-reduced Temp sigma-coll)
    "Hard-sphere collision integral Omega-11 as function of
`temp' (Kelvin), `m-reduced' (amu), `sigma-coll' (angstrom) (Ferziger
& Kaper 7.1-28)

For collisions between species a and b the arguments are (Ferziger &
Kaper, Sect 9.2):

m-reduced <- (/ (* m-a m-b)
                (+ m-a m-b))
sigma-coll <- (* 0.5 (+ sigma-a sigma-b))

For a signle species, this translates into:
m-reduced <- (/ m 2)
sigma-coll <- sigma

In that case, the formula below reduces to something similar to Kee et
al, (12.95):
  (* (sqrt (/ (* +pi+ +kb+ Temp)
	      (* m-reduced)))
     (^2 sigma))
The Kee formula is evidently a incomplete simplification with syntax errors"
    (* +omega-hs-constant+
       (sqrt (/ Temp m-reduced))
       (^2 sigma-coll))))

(define-test omega-LS-hs*
  (assert-equal 1 (omega-LS-hs* 1 1))
  (assert-equal 3 (omega-LS-hs* 1 2))
  (assert-equal 12 (omega-LS-hs* 1 3))
  (assert-equal 2 (omega-LS-hs* 2 2)))

(define-test Omega-11-hs1
  (assert-number-equal +omega-hs-constant+  (omega-11-hs1 1d0 1d0 1.0d0) "Basic")
  (assert-number-equal (/ +omega-hs-constant+ 2)  (omega-11-hs1 4.0 1.0 1.0) "Mass test")
  (assert-number-equal (* +omega-hs-constant+ 4)  (omega-11-hs1 1.0 1.0 2.0) "Sigma test")
  (assert-number-equal (* +omega-hs-constant+ 2) (omega-11-hs1 1.0 4.0 1.0) "Temperature test"))


(defun Omega-12-hs1 (m-reduced Temp sigma-coll)
  (* (omega-ls-hs* 1 2)
     (omega-11-hs1 m-reduced Temp sigma-coll)))

(defun Omega-13-hs1 (m-reduced Temp sigma-coll)
  (* (omega-ls-hs* 1 3)
     (omega-11-hs1 m-reduced Temp sigma-coll)))

(defun Omega-14-hs1 (m-reduced Temp sigma-coll)
  (* (omega-ls-hs* 1 4)
     (omega-11-hs1 m-reduced Temp sigma-coll)))

(defun Omega-15-hs1 (m-reduced Temp sigma-coll)
  (* (omega-ls-hs* 1 5)
     (omega-11-hs1 m-reduced Temp sigma-coll)))

(defun Omega-22-hs1 (m-reduced Temp sigma-coll)
  (* (omega-ls-hs* 2 2)
     (omega-11-hs1 m-reduced Temp sigma-coll)))

(defun omega-ij-hs1 (i j mass temperature sigma)
  (* (omega-ls-hs* i j)
     (omega-11-hs1 mass temperature sigma)))



;; Now the explicit hard sphere integrals.  I use a macro to ease the
;; multiple definitions as they all use a scaling.
(defmacro defomega-ij-hs (i j)
  "Define the generic function and method for calculating hard-sphere
collision integrals"
  (let ((si (prin1-to-string i))
	(sj (prin1-to-string j)))
    (let ((gf-name (alexandria:symbolicate "OMEGA-" si sj "-HS")))
    `(defgeneric ,gf-name (potential-parameters temp)
       (:documentation
	,(format nil "Evaluate the omega-~a~a collision integral for the
hard-sphere potential specified by `potential-parameters' at tempeature
`temp'

`potential-parameters' is an object of class `hard-sphere-potential'"
		i j))
       (:method ((model hard-sphere-potential) temp)
	 (with-slots (mass sigma) model
	   (* (omega-LS-hs* ,i ,j)
	      (omega-11-hs1 mass temp sigma))))))))

(define-test defomega-ij-hs
  (assert-expands
   '(defgeneric omega-11-hs (potential-parameters temp)
     (:documentation "Evaluate the omega-11 collision integral for the
hard-sphere potential specified by `potential-parameters' at tempeature
`temp'

`potential-parameters' is an object of class `hard-sphere-potential'")
     (:method ((model hard-sphere-potential) temp)
       (with-slots (mass sigma) model
	 (* (omega-LS-hs* 1 1)
	    (omega-11-hs1 mass temp sigma)))))
   (defomega-ij-hs 1 1)))




(defomega-ij-hs 1 1)
(defomega-ij-hs 1 2)
(defomega-ij-hs 1 3)
(defomega-ij-hs 1 4)
(defomega-ij-hs 1 5)
(defomega-ij-hs 2 2)

(define-test Omega-xy-hs
  (let ((temp (/ (* +pi+ +kb+)))
	(hsp (make-hard-sphere-potential 0.5 1.0)))
    (assert-number-equal (* 3.0 (omega-11-hs hsp Temp))
			 (Omega-12-hs  hsp Temp))
    (assert-number-equal (* 12.0 (omega-11-hs hsp Temp))
			 (Omega-13-hs  hsp Temp))
    (assert-number-equal (* 2.0 (omega-11-hs hsp Temp))
			 (Omega-22-hs  hsp Temp))))



