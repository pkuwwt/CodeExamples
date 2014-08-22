!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  @file      mod_struct.f95
!  @Author    Mitch Richling<http://www.mitchr.me>
!  @Copyright Copyright 2012 by Mitch Richling.  All rights reserved.
!  @brief     The basic structure of a module file.@EOL
!  @Std       F95

MODULE mod_struct
  IMPLICIT NONE  

  INTEGER, PUBLIC  :: modint  ! PUBLIC is the default, but I like to say it anyway
  INTEGER, PRIVATE :: i       ! This I variable will not collide with I in the main program

  PUBLIC  :: hi_ma            ! PUBLIC is the default, but I like to say it anyway

CONTAINS

  SUBROUTINE hi_ma
    WRITE (*,*) 'hello ma'
  END SUBROUTINE hi_ma

END MODULE mod_struct
