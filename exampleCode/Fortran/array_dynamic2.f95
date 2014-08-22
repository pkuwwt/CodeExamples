!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  @file      array_dyn_allocatable.f95
!  @Author    Mitch Richling<http://www.mitchr.me>
!  @Copyright Copyright 2012 by Mitch Richling.  All rights reserved.
!  @brief     Dynamic, allocatable, arrays.@EOL
!  @Std       F95

PROGRAM array_dyn_allocatable
  IMPLICIT NONE

  INTEGER                           :: ierror          ! Used error code
  REAL, ALLOCATABLE, DIMENSION(:)   :: vector          ! No space is allocated here
  REAL, ALLOCATABLE, DIMENSION(:,:) :: matrix          ! No space is allocated here

  IF(.NOT. ALLOCATED(vector)) THEN                     ! Allocate / error check
     WRITE (*,*) 'Allocating space for vector now..'
      ALLOCATE(vector(100), STAT=ierror)
      IF(ierror .NE. 0) THEN                           ! One way to check the result of ALLOCATE
         WRITE (*,*) 'ERROR: Could not allocate vector'
         STOP
      END IF
  END IF

  IF(ALLOCATED(vector)) THEN                           ! Deallocate the space
     WRITE (*,*) 'Deallocating space for vector now..'
      DEALLOCATE(vector)
  END IF
  
  ALLOCATE(vector(123), matrix(0:2,1:7))               ! Multiple objects may be allocated/deallocated with one call.
  DEALLOCATE(matrix, vector)                           ! Not Required. In f95, arrays are deallocated at end of scope unless SAVEd.
END PROGRAM array_dyn_allocatable
