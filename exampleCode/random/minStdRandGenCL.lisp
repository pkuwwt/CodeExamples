#!/usr/local/bin/clisp
;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:132 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @file      minStdRandGenCL.lisp
;; @author    Mitch Richling <http://www.mitchr.me>
;; @Copyright Copyright 1998 by Mitch Richling.  All rights reserved.
;; @brief     minimal implementation of the minimal standard random number generator@EOL
;; @Keywords  
;; @Std       Common Lisp
;;
;;            See the C version for extensive algorithm notes.
;;
;;            To run: clisp -norc -q minStdRandGenCL.lisp
;;            

;;----------------------------------------------------------------------------------------------------------------------------------
(let* ((m      2147483647)      ; 2**(31)-1 for MSLCG               
       (a      16807)           ; 7**5 for MSLCG                    
       (q      (truncate m a))  ; 127773 for MSLCG                  
       (r      (mod m a))       ;   2836 for MSLCG                  
       (randN  1))              ; Set the seed (first random number)
  (loop for i from 1 to 10 do
    (format 't "~12d~1%" 
            (setq randN
                  (let ((tmpRandN (- (* a (mod randN q)) (* r (truncate randN q)))))
                  (if (< tmpRandN 0) (+ tmpRandN m) tmpRandN))))))
