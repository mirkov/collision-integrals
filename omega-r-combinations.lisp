;; Mirko Vukovic
;; Time-stamp: <2012-05-15 14:04:59 omega-r-combinations.lisp>
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

;;; Functions A*, B*, C*, D*, E* from Ferziger & Kaper, Section 7.1,
;;; p. 207.  As noted in the text, these are the same as those of
;;; Hirshfelder et al, and differ from Chapman & Cowling 1952.  I was
;;; unable to locate the definitions in Chapman & Cowling, Third Ed.
;;; The closest are the coefficients A, B, C, E in Section 9.8


(export '(A* B* C* E* F*))
(defun A* (T*)
  "Ferziger & Kaper, 7.1-31"
  (/ (omega*-22 T*) (omega*-11 T*)))

(defun B* (T*)
  "Ferziger & Kaper, 7.1-32"
  (/ (- (* 5 (omega*-12 T*))
	(* 4 (omega*-13 T*)))
     (omega*-11 T*)))

(defun C* (T*)
  "Ferziger & Kaper, 7.1-33"
  (/ (omega*-12 T*) (omega*-11 T*)))

(defun E* (T*)
  "Ferziger & Kaper, 7.1-34"
  (/ (omega*-23 T*) (omega*-22 T*)))

(defun F* (T*)
  "Ferziger & Kaper, 7.1-35"
  (/ (omega*-33 T*) (omega*-11 T*)))