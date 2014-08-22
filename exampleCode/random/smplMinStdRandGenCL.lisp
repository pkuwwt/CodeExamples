#!/usr/local/bin/clisp
;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:132 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      smplMinStdRandGenCL.lisp
;; @author    Mitch Richling <http://www.mitchr.me/>
;; @Copyright Copyright 1998 by Mitch Richling.  All rights reserved.
;; @brief     minimal implementation of the minimal standard random number generator@EOL
;; @Keywords  
;; @Std       Common Lisp
;;
;; Simple implementation of the minimal standard random number generator making use of the Lisp's arbitrary precision arithmetic.
;; See the alternate implementation with 32-bit arithmetic found in minStdRandGenCL.lisp.
;;
;; To run: clisp -norc -q smplMinStdRandGenCL.lisp

;;----------------------------------------------------------------------------------------------------------------------------------
(let* ((m      2147483647)      ; 2**(31)-1 for MSLCG               
       (a      16807)           ; 7**5 for MSLCG                    
       (b      0)
       (randN  1))              ; Set the seed (first random number)
  (loop for i from 1 to 10 do
    (format 't "~12d~1%" (setq randN (mod (+ (* randN a) b) m)))))
