;; Mirko Vukovic
;; Time-stamp: <2011-09-06 20:33:33 lj-collision-integral-fits.lisp>
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

;; Fits of reduced collision integrals

;; We offer Omega-11,22 2-exponent fits from Kee and for log T*
;; polynomial fit for Omega22 from Lemmon&Jacobsen.  The latter one
;; has a rather fails outside the bounds of fit, while the former
;; seems to extrapolate the data rather well.


(defclass 2exponent-omega*-fit ()
  ((c1 :initarg :c1
       :reader c1)
   (c2 :initarg :c2
       :reader c2)
   (c3 :initarg :c3
       :reader c3)
   (c4 :initarg :c4
       :reader c4)
   (doc :initarg :doc
		  :reader doc
		  :documentation "Document the coeffs"))
  (:documentation "Class for storing coefficients of for two-exponent
fits to Omega*:

Omega*(T*)=c1 T*^(-c2)+(T*+c3)^(-c4)"))

(defmethod print-object ((self 2exponent-omega*-fit) stream)
  (print-unreadable-object (self stream :type t :identity t)))

(defmethod describe-object ((self 2exponent-omega*-fit) stream)
  (with-slots (c1 c2 c3 c4 doc) self
    (format stream doc)
    (format stream "c1 = ~a
c2 = ~a
c3 = ~a
c4 = ~a~%" c1 c2 c3 c4)))

;; 2-term exponent formulae for Omega from Kee et al
(defmethod omega-2exponent-fit ((self 2exponent-omega*-fit) T*)
  "omega fitting function Kee et al, (12.6,7)"
  (with-slots  (c1 c2 c3 c4) self
    (+ (* c1 (expt T* (- c2)))
       (expt (+ T* c3) (- c4)))))


(defun make-2exponent-omega*-fit-obj (c1 c2 c3 c4 doc)
  (make-instance '2exponent-omega*-fit
		 :c1 c1 :c2 c2 :c3 c3 :c4 c4 :doc doc))

(defconstant *omega*-11-2exponent-fit*
  (make-2exponent-omega*-fit-obj
   1.0548 0.15504 0.55909 2.1705
   "Coefficients of the Omega*-11 2-exponent fit (Kee et al)")
  "Two exponent fit to Omega*-11")

(defconstant *omega*-22-2exponent-fit*
  (make-2exponent-omega*-fit-obj
   1.0548 0.15504 0.55909 2.1705
   "Coefficients of the Omega*-22 2-exponent fit (Kee et al)")
  "Two exponent fit to Omega*-22")

(defmethod omega*%-11 ((self (eql *omega*-11-2exponent-fit*)) T*)
  (omega-2exponent-fit self T*))

(defmethod omega*%-22 ((self (eql *omega*-22-2exponent-fit*)) T*)
  (omega-2exponent-fit self T*))

;; 4-term log/exp expansion from Lemmon & Jacobsen
(defclass LJ04-omega-22-coeffs ()
  ((coeffs-vector :initform #(0.431 -0.4623 0.08406 0.005341 -0.00331)
		  :documentation "Fit coefficients"))
  (:documentation
"A container of coefficients for calculating the Omega-22 collision integral
using the log/polynomial expansion of Lemon & Jacobsen, 2004

Agreement with the two exponent expansion breaks down for T* < 0.2 and T* > 4"))



(defmethod print-object ((self 2exponent-omega*-fit) stream)
  (print-unreadable-object (self stream :type t :identity t)))

(defmethod describe-object ((self 2exponent-omega*-fit) stream)
  (with-slots (doc) self
    (format stream doc)))

(defun make-lj04-omega-22-coeffs ()
  (make-instance 'lj04-omega-22-coeffs))

(defconstant *omega*-22-logT*-poly-fit*
  (make-lj04-omega-22-coeffs)
  "Omega-22 fit using a polynomial in log T* from Lemmon and Jacobsen, 2004")

(defmethod omega*%-22 ((coeff-container (eql *omega*-22-logT*-poly-fit*)) T*)
    (let ((logT* (log T*)))
      (exp (polyeval logT* (slot-value coeff-container 'coeffs-vector)))))


