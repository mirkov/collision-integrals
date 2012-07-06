;; Mirko Vukovic
;; Time-stamp: <2011-09-01 18:55:12 setup-explicit-and-default-functions.lisp>
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

;; setup the generic functions so that we can add methods in the
;; methods module.  Also set-up the default calling functions

(defgeneric omega-11*% (model T*)
  (:documentation "Evaluate the omega-11 `model' at reduced-temperature T*
If model is specified as `t', use default model")
  (:method ((model (eql t)) reduced-temperature)
    (funcall #'omega-11*% (default-omega-calc-method :omega*-11)
	     reduced-temperature)))

(defun omega-11* (T*)
  "Evaluate omega-11 at T* using the default method"
  (omega-11*% t T*))

(defgeneric omega-12*% (model T*)
  (:documentation "Evaluate the omega-12 `model' at reduced-temperature T*
If model is specified as `t', use default model")
  (:method ((model (eql t)) reduced-temperature)
    (funcall #'omega-12*% (default-omega-calc-method :omega*-12)
	     reduced-temperature)))

(defun omega-12* (T*)
  "Evaluate omega-12 at T* using the default method"
  (omega-12*% t T*))

(defgeneric omega-13*% (model T*)
  (:documentation "Evaluate the omega-13 `model' at reduced-temperature T*
If model is specified as `t', use default model")
  (:method ((model (eql t)) reduced-temperature)
    (funcall #'omega-13*% (default-omega-calc-method :omega*-13)
	     reduced-temperature)))

(defun omega-13* (T*)
  "Evaluate omega-13 at T* using the default method"
  (omega-13*% t T*))

(defgeneric omega-14*% (model T*)
  (:documentation "Evaluate the omega-14 `model' at reduced-temperature T*
If model is specified as `t', use default model")
  (:method ((model (eql t)) reduced-temperature)
    (funcall #'omega-14*% (default-omega-calc-method :omega*-14)
	     reduced-temperature)))

(defun omega-14* (T*)
  "Evaluate omega-14 at T* using the default method"
  (omega-14*% t T*))

(defgeneric omega-15*% (model T*)
  (:documentation "Evaluate the omega-15 `model' at reduced-temperature T*
If model is specified as `t', use default model")
  (:method ((model (eql t)) reduced-temperature)
    (funcall #'omega-15*% (default-omega-calc-method :omega*-15)
	     reduced-temperature)))

(defun omega-15* (T*)
  "Evaluate omega-15 at T* using the default method"
  (omega-15*% t T*))

(defgeneric omega-22*% (model T*)
  (:documentation "Evaluate the omega-22 `model' at reduced-temperature T*
If model is specified as `t', use default model")
  (:method ((model (eql t)) reduced-temperature)
    (funcall #'omega-22*% (default-omega-calc-method :omega*-22)
	     reduced-temperature)))

(defun omega-22* (T*)
  "Evaluate omega-22 at T* using the default method"
  (omega-22*% t T*))