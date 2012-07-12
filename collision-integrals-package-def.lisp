;; Mirko Vukovic
;; Time-stamp: <2012-07-11 17:48:13 collision-integrals-package-def.lisp>
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


(defpackage :collision-integrals
  (:nicknames :omega-xx)
  (:use :cl  :lisp-unit)
  (:import-from :alexandria
		:with-input-from-file
		:symbolicate)
  (:import-from :my-utils :polyeval :^2)
  (:import-from :physics-constants :+boltzmann-constant-sp+)
  (:import-from :mv-grid+gsll :tabular-data :make-table
		:init-table-interp :interp-table)
  (:import-from :molecular-potentials
		:species :mass :sigma :epsilon/K
		:molecular-potential
		:make-lennard-jones-6/12-potential
		:make-species-lennard-jones-6/12-potential
		:lennard-jones-6/12-potential
		:make-hard-sphere-potential
		:hard-sphere-potential)
  (:documentation "Package for calculating collision integrals for collisions in low pressure gases.
For documentation and syntax, see the html file in the doc directory.")
  (:export
   ;; Reduced collision integrals for the Lennard-Jones 6-12 potential
   ;; as function of reduced temperature T*, calculated using the
   ;; default methods stored in *omega*-calc-defaults*
   :omega*-11 :omega*-12 :omega*-13 :omega*-14 :omega*-15 :omega*-22
   :omega*-23 :omega*-24 :omega*-25 :omega*-26 :omega*-33 :omega*-44
   ;; Reduced collision integrals for the Lennard-Jones 6-12 potential
   ;; as function of reduced temperature T* (passed as second
   ;; argument), calculated using the method passed as first argument.
   ;; If the first argument is `t', the default calculation method is
   ;; used.
   :omega*%-11 :omega*%-12 :omega*%-13 :omega*%-14 :omega*%-15 :omega*%-22
   ;; Collision integrals are products of the hard-sphere collision
   ;; integral and the reduced collision integral.  The reduced
   ;; collision integral is calculated using the default method for
   ;; the Lennard-Jones 6-12 potential.
   :omega-11 :omega-12 :omega-13 :omega-14 :omega-15 :omega-22
   ;; Collision integrals of order I,J for the hard-sphere potential
   ;; whose argument is an object of `collision-parameters' class
   :omega-11-hs :omega-12-hs :omega13-hs :omega14-hs :omega15-hs :omega22-hs
   :omega-ij-hs
   ;; functions for hard-sphere collision integrals, whose
   ;; arguments are the reduced mass, sigma, temperature
   :omega-11-hs1 :omega-12-hs1 :omega-13-hs1 :omega-14-hs :omega-15-hs1 :omega-22-hs1
   :omega-ij-hs1
   ;;Calculation defaults for Lennard-Jones reduced integrals
   :*omega*-lj-calc-defaults* :default-omega*-lj-calc-method :set-default-omega*-lj-calc-method
   ;; A few reduced collision integrals have been fitted with analytic
   ;; expessions.  The fit coefficients are stored in the following
   ;; variables, and can be invoked by passing them to omega-xy*%
   ;; methods
   :*omega*-11-2exponent-fit* :*omega*-22-2exponent-fit* :*omega*-22-logT*-poly-fit*
   ;; Omega integrals are extensively tabulated by Ivchenko et al.
   ;; The following variable holds the object with the table.  It also
   ;; triggers interpolation to obtain the desired values.
   :*omega*-table*
   :make-collision-parameters :mass :sigma :epsilon/K))






