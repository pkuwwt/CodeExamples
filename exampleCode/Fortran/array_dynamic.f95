!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  @file      array_dynamic.f95
!  @Author    Mitch Richling<http://www.mitchr.me>
!  @Copyright Copyright 2012 by Mitch Richling.  All rights reserved.
!  @brief     Demonstrate automatic and allocated arrays.@EOL
!  @Keywords  none
!  @Std       F95
!
!             The allocated technique below works well for arrays that are too big to fit on the stack.
!             For another example of allocated, see dynamic_array.f95.
!             
!             The swap subroutines here are quite wasteful in terms of RAM, see array_elemental.f95 for a better implementation.

PROGRAM array_dynamic

  IMPLICIT NONE

  INTEGER, DIMENSION(4) :: i = (/ 1, 2, 3, 4 /) 
  INTEGER, DIMENSION(4) :: j = (/ 4, 3, 2, 1 /) 

  PRINT *, 'i=', i
  PRINT *, 'j=', j
  CALL swap1(i, j)
  PRINT *
  PRINT *, 'i=', i
  PRINT *, 'j=', j
  CALL swap2(i, j)
  PRINT *
  PRINT *, 'i=', i
  PRINT *, 'j=', j
  CALL swap3(i, j)
  PRINT *
  PRINT *, 'i=', i
  PRINT *, 'j=', j

CONTAINS

  SUBROUTINE swap1(a, b)
    IMPLICIT NONE
    INTEGER, DIMENSION(:), INTENT(INOUT) :: a, b      ! assumed size arrays    
    INTEGER, DIMENSION(SIZE(a)) :: tmp                ! space allocated (normally on stack) here
    tmp = a
    a = b
    b = tmp
                                                      ! Space for tmp is automatically released now
  END SUBROUTINE swap1

  SUBROUTINE swap2(a, b)
    INTEGER                              :: ierror    ! Used error code
    INTEGER, DIMENSION(:), INTENT(INOUT) :: a, b      ! assumed size arrays
    INTEGER, DIMENSION(:), ALLOCATABLE   :: tmp       ! No space is allocated here
    ALLOCATE(tmp(SIZE(a)), STAT=ierror)               ! allocate space (normally on the heap)
    IF(ierror .NE. 0) THEN
       PRINT *, 'ERROR: Could not allocate vector'    ! die if we could not allocate space
       STOP
    END IF
    IF (.NOT.ALLOCATED(tmp)) THEN                     ! die if we could not allocate space
       PRINT *, 'ERROR: Could not allocate vector'      ! It is not necessary to test this after the previous
       STOP                                             ! IF/THEN, but it is a good demonstration of the ALLOCATE
    END IF                                              ! function. :)
    tmp = a
    a = b
    b = tmp
    DEALLOCATE(tmp)                                   ! Not required.  Here to remind us that space for tmp released
                                                      ! when it falls out of scope (no SAVE attribute on tmp).
  END SUBROUTINE swap2

  SUBROUTINE swap3(a, b)
    IMPLICIT NONE
    INTEGER, DIMENSION(:), INTENT(INOUT) :: a, b      ! assumed size arrays
    INTEGER, DIMENSION(:), POINTER :: tmp             ! No space is allocated here
    ALLOCATE(tmp(SIZE(a)))                            ! allocate space (normally on the heap)
    if (.NOT.ASSOCIATED(tmp)) stop                    ! die if we could not allocate space
    tmp = a
    a = b
    b = tmp
    DEALLOCATE(tmp)                                   ! REQUIRED (pointers don't get automatic cleanup)
  END SUBROUTINE swap3

END PROGRAM array_dynamic
