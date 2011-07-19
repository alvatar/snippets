;;; -*-scheme-*-
;;; vctlib.scm
;;;
;;; vctlib module (load in your program)
;;; 
;;; Copyright 2005-2007 Pierre-Alexandre Fournier
;;; (fournier@carretechnologies.com)
;;; All rights reserved.
;;; 
;;; $Id: $


;;; Requirements:
(cond-expand 
 (gambit
  (load "../blas/blas-gambit-ffi"))
 (chicken
  (require blas)))


;;; base
(include "../gambit-lib/wrappers.scm")
(include "../srfi/srfi-1.scm")
(load "utils")
(load "vct")
(load "vct-C")
(load "vct-blas")
(load "matrix")
(load "matrix-ndim")
(load "ppm")

