!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  @file      variable_decl.f95
!  @Author    Mitch Richling<http://www.mitchr.me>
!  @Copyright Copyright 2012 by Mitch Richling.  All rights reserved.
!  @brief     Basic variable declarations.@EOL
!  @Std       F95

PROGRAM variable_decl
  IMPLICIT NONE

  ! General Syntax:
  !   basic_type (KIND=NN), attribute,... :: var1, var2,...
  !   basic_type: integer, real, complex, character, logical
  !   Attributes:
  !      ALLOCATABLE -- no memory used here, allocate later
  !      DIMENSION   -- array                              
  !      EXTERNAL    -- defined outside this compilation   
  !      INTENT      -- argument intent: in, inout or out  
  !      OPTIONAL    -- argument is optional               
  !      INTRINSIC   -- declaring a function as intrinsic  
  !      PARAMETER   -- declaring a constant               
  !      POINTER     -- declaring a pointer                
  !      PRIVATE     -- private declaration (in a module)  
  !      PUBLIC      -- public declaration (in a module)   
  !      SAVE        -- keep value from between calls (in a function or subroutine)
  !      TARGET      -- can be pointed to by a pointer     


  ! This also simplifies modifying the program, should the value of pi change.  -- Early FORTRAN manual for Xerox Computers
  REAL,    PARAMETER :: my_pi = 3.14
  INTEGER            :: two   = 2

  WRITE (*,*) my_pi, two

END PROGRAM variable_decl
