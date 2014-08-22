! -*- Mode:F90; Coding:us-ascii-unix; fill-column:132 -*-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!  @file      ranlibM.f90
!  @Author    Mitch Richling<http://www.mitchr.me/>
!  @Copyright Copyright 1997 by Mitch Richling.  All rights reserved.
!  @breif     F90 module with interface for RANLIB package@EOL
!  @Keywords  ranlib fortran module
!  @Std       F90
!
!  Note that this interface set is NOT complete.  I just add interfaces for the routines as I first use them.

!-----------------------------------------------------------------------------------------------------------------------------------
MODULE ranlibM

  INTERFACE

     real function GENNOR(m, sd)
       REAL, intent(in) :: m
       REAL, intent(in) :: sd
     END function GENNOR

     real function RANF()
     end function RANF

     integer function IGNLGI()
     end function IGNLGI

     subroutine SETALL(seed1, seed2)
       integer, intent(in) :: seed1, seed2
     end subroutine SETALL

     subroutine SETSD(seed1, seed2)
       integer, intent(in) :: seed1, seed2
     end subroutine SETSD

     subroutine GETSD(seed1, seed2)
       integer, intent(in) :: seed1, seed2
     end subroutine GETSD

     subroutine SETCGN(g)
       integer, intent(in) :: g
     end subroutine SETCGN

     subroutine GETCGN(g)
       integer, intent(out) :: g
     end subroutine GETCGN

  END INTERFACE

end MODULE ranlibM
