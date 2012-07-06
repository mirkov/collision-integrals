;; Mirko Vukovic
;; Time-stamp: <2012-05-13 21:33:02 collision-integrals-user.lisp>
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

(in-package :collision-integrals-user)

(defun foo (format-string &rest args)
  (apply #'format t (format nil "~a~%" format-string) args))

(defun demo ()
  (setf (default-omega*-lj-calc-method :Omega*-11) *omega*-table*)
  (foo "We first set-up the problem")
  (let ((temp 300d0)
	(first-element :AR)
	(other-element :H))
    (foo "We demonstrate reduced collision integrals for ~a and ~:*~a/~a mixture at ~aK~%"
	 first-element other-element temp)
    (let* ((Ar-lj (make-species-lennard-jones-6/12-potential first-element))
	   (T*-argon (/ temp (epsilon/K Ar-lj))))
      (foo "We create an object ~a holding coefficients for the LJ potantial for ~a~%"
	   Ar-lj first-element)
      (foo "The potential parameters are printed with `describe'")
      (describe Ar-lj)
      (foo "The reduced temperture for Ar is ~a~%" T*-argon)
      (foo "The reduced collision integral Omega 12 for T* is ~a~%" (omega*-11 T*-argon))
      (foo "~%Next, we calculate the reduced collision integral for a gas mixture
We get the coefficients for ~a/~a mixture" first-element other-element)
      (foo "This is accomplished by calling `make-lj-coeffs :~a :~a'" first-element other-element)
      (foo "However, a mixture of two gases has collisions between like gas molecules and
unlike gas molecules.  We will therefore also get the coefficients for ~a" other-element)
      (let* ((H2-lj (make-species-lennard-jones-6/12-potential  :H2))
	     (combo-lj (make-collision-parameters Ar-lj H2-lj))
	     (T*-H2 (/ temp (epsilon/k H2-lj)))
	     (T*-AR+H2 (/ temp (epsilon/k combo-lj))))
	(describe H2-lj)
	(describe combo-lj)
	(foo "The reduced temperature for A/H2 is ~a" T*-H2)
	(foo "The reduced collision integral-11 for Ar/H2 is ~a~%" (omega*-11 T*-Ar+H2))
	(foo "Next we demonstrate changing the default calculation method for Omega* to the 2-exponent fit")
	(let ((collision-integrals::*omega*-11-default* collision-integrals:*omega*-11-2exponent-fit*))
	  (foo "The reduced collision integral-11 is now Ar/H2 is ~a~%" (omega*-11 T*-Ar+H2))))
      (foo "So far, we were dealing with reduced collision integrals.")
      (foo "We now move to collision integrals")
      (let ((mass-amu (mass Ar-lj))
	    (sigma-A (sigma Ar-lj)))
	(foo "For ~a, we use the mass ~a amu and diameter ~a angstrom" first-element mass-amu sigma-A)
	(foo "The hard-sphere collision integral is ~a"
	     (omega-11-hs (make-hard-sphere-potential mass-amu sigma-A) temp))
	(foo "The LJ collision integral is ~a"
	     (omega-11 Ar-lj temp))
	(foo "Finally, a practical application, viscosity of ~a" first-element)
	(foo "We compare with value from Lemmon & Jacobsen of ~a Pa.s" 22.72e-6)
	(foo "We need to create the Ar-Ar collision parameters")
	(let* ((cp-Ar-Ar (make-collision-parameters Ar-lj)))
	  (print cp-Ar-Ar)
	  (describe cp-Ar-Ar)
	  (foo "We first use the form in terms of reduced integrals, Ferziger & Kaper,(7.3-6b)")
	  (foo "noting that mass is not reduced mass, but molecular mass (see text around 7.1-29)")
	  (let* ((mass (mass cp-ar-ar))
		 (sigma (sigma cp-ar-ar))
		 (t* (/ temp (epsilon/k cp-ar-ar)))
		 (coeff (sqrt (* +pi+ (* +amu+ 1e20) (* +kb+ 1e20))))
		 (nux (* (/ 5 16)
			 coeff
			 (/ (sqrt (* 2 mass temp))
			    (* +pi+ (expt sigma 2) (omega*-22 t*))))))
	    (foo "We get for Ar viscosity at ~a K ~a Pa.s" temp nux))
	  (foo "Next we use the form in terms of the full integrals, F&P 7.3-61")
	  (let ((nu (/ (* 5 +kb+ temp)
		       (* 8 (omega-22 cp-Ar-Ar temp)))))
	    (foo "We get for Ar viscosity at ~a K ~a Pa.s" temp nu)
	    (foo "The relative difference is ~a" (/ (- nu 22.72e-6) 22.72e-6))))))))
