;; Mirko Vukovic
;; Time-stamp: <2012-04-25 15:43:45 omega-gf+defaults.lisp>
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

(defmacro defomega-ij (i j)
  "Defines generic function omega-ij, its documentation, and two
methtods.

For full expansion, see definition of unit-test
`defomega-ij'"
  (let ((si (prin1-to-string i))
	(sj (prin1-to-string j)))
  (let ((gf-name (alexandria:symbolicate "OMEGA-" si sj))
	(omega-hs-name (alexandria:symbolicate "OMEGA-" si sj "-HS"))
	(omega*-name (alexandria:symbolicate "OMEGA*-" si sj)))
    `(defgeneric ,gf-name (collision-params temperature)
       (:documentation ,(format nil "Evaluate the omega-~a~a integral for
`collision-params and `temperature' and

`collision-params' is an object of base type `molecular-potential' or
one of its subclasses:
- hard-sphere-potential
- lennard-jones-6/12-potential
or of a collision class (tbd).

These provide the slot values:
- mass
- sigma
- epsilon/K

Kee et al, Sect. 12.4.2" i j))
       (:method ((hs-potential hard-sphere-potential) temperature)
	 (funcall #',omega-hs-name hs-potential temperature))
       (:method ((hs-potential hard-sphere-collision-parameters) temperature)
	 (funcall #',omega-hs-name hs-potential temperature))
       (:method ((lj-potential lennard-jones-6/12-potential) temperature)
	 (with-slots (mass sigma epsilon/k) lj-potential
	   (* (funcall #',omega-hs-name
		       (make-hard-sphere-potential mass sigma)
		       temperature)
	      (funcall #',omega*-name (/ temperature epsilon/k)))))))))

(define-test defomega-ij
  (assert-expands
   '(defgeneric omega-11 (collision-params temperature)
     (:documentation   "Evaluate the omega-11 integral for
`collision-params and `temperature' and

`collision-params' is an object of base type `molecular-potential' or
one of its subclasses:
- hard-sphere-potential
- lennard-jones-6/12-potential
or of a collision class (tbd).

These provide the slot values:
- mass
- sigma
- epsilon/K

Kee et al, Sect. 12.4.2")
     (:method ((hs-potential hard-sphere-potential) temperature)
       (funcall #'omega-11-hs hs-potential temperature))
     (:method ((lj-potential lennard-jones-6/12-potential) temperature)
       (with-slots (mass sigma epsilon/k)  lj-potential
	 (* (funcall #'omega-11-hs
		     (make-hard-sphere-potential mass sigma)
		     temperature)
	    (funcall #'omega*-11 (/ temperature epsilon/k))))))
   (defomega-ij 1 1)))

(defomega-ij 1 1)
(defomega-ij 1 2)
(defomega-ij 1 3)
(defomega-ij 1 4)
(defomega-ij 1 5)
(defomega-ij 2 2)


