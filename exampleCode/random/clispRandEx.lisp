#!/usr/local/bin/clisp
;; -*- Mode:Lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:132 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      clispRandEx.lisp
;; @author    Mitch Richling <http://www.mitchr.me/>
;; @Copyright Copyright 2004 by Mitch Richling.  All rights reserved.
;; @brief     How to use the Common Lisp random number generator.@EOL
;; @Keywords  none
;; @Std       Common Lisp
;;
;; Like most languages, Common Lisp has a standard random number generator.  Also like most languages, the algorithm is not
;; specified in the standard, and is not usually of high enough quality to use for simulation work.

;;----------------------------------------------------------------------------------------------------------------------------------
;; The current random state:
*random-state*

;;----------------------------------------------------------------------------------------------------------------------------------
;; You can tell if things are really a random state this way
(random-state-p *random-state*)

;;----------------------------------------------------------------------------------------------------------------------------------
;; 10 random numbers in U[0,100) using the current random state (*random-state*).  In Common Lisp, the numbers are of the same type
;; as the argument to random; however, in some versions of lisp only integers are allowed (elisp for example).  Also note that in
;; elisp the limit is optional, and if omitted any representable integer may be returned.
(loop for i from 1 to 10 collect (random 100))

;;----------------------------------------------------------------------------------------------------------------------------------
;; How about 10 floating point numbers (this won't work with elisp):
(loop for i from 1 to 10 collect (random 100.0))

;;----------------------------------------------------------------------------------------------------------------------------------
;; One can make a new random state (arg 't), copy a state (arg a state), or return the current state (arg is nil or missing) with
;; make-random-state.  In elisp, this function sets *random-state* as a side effect!  Let's make a new state just for fun:
(setq aNewRandomState (make-random-state 't))

;;----------------------------------------------------------------------------------------------------------------------------------
;; If the argument to make-random-state is a random state, then a copy of the given state will be returned:
(setq anotherRandomState (make-random-state aNewRandomState))

;;----------------------------------------------------------------------------------------------------------------------------------
;; One uses a random state via an optional argument to random.  Note that elisp doesn't support the second argument.
(loop for i from 1 to 10 collect (random 100 aNewRandomState))

;;----------------------------------------------------------------------------------------------------------------------------------
;; We can get the same sequence with the copy of the state we made.  Again, this won't work in elisp.
(loop for i from 1 to 10 collect (random 100 anotherRandomState))
