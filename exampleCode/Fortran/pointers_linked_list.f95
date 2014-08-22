!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  @file      pointers_linked_list.f95
!  @Author    Mitch Richling<http://www.mitchr.me>
!  @Copyright Copyright 2012 by Mitch Richling.  All rights reserved.
!  @brief     Pointer demo with a linked list. @EOL
!  @Std       F95

PROGRAM pointers_linked_list

  IMPLICIT NONE

  TYPE node
     TYPE(node), POINTER :: next
     CHARACTER(LEN=100)  :: data
  END TYPE node

  TYPE(node), POINTER :: the_list
  the_list => NULL()
  CALL dump
  CALL push('foo')
  CALL dump
  CALL push('bar')
  CALL dump
  CALL push('foobar')
  CALL dump

CONTAINS

  SUBROUTINE push(string)                                                  ! Push a string on the list
    IMPLICIT NONE
    CHARACTER(len=*), INTENT(IN)  :: string
    TYPE(node), POINTER           :: new_node                              ! The node we will add
    ALLOCATE(new_node)                                                     ! Allocate space for the new node
    IF(.NOT.(ASSOCIATED(new_node))) THEN                                   ! Make sure the allocation worker
       PRINT *, 'Could not allocate node'
       STOP
    END IF
    new_node%data = string                                                 ! Copy the input string into our new node
    IF(ASSOCIATED(the_list)) THEN                                          ! If the list is not empty
       new_node%next => the_list                                             ! we set the next node to the previous first node
    ELSE
       new_node%next => NULL()                                               ! else we set it to NULL
    END IF
    the_list => new_node                                                   ! Link the new node at the top of the list
    RETURN
  END SUBROUTINE push

  SUBROUTINE dump()                                                        ! Print out the list
    IMPLICIT NONE
    TYPE(node), POINTER :: cur_node
    cur_node => the_list                                                   ! Start with the first element
    WRITE (*,FMT="(a)",ADVANCE='NO') 'LIST: '                              ! Print the list tag
    DO
       IF(ASSOCIATED(cur_node)) THEN                                       ! Check to see if we have a node
          WRITE (*,FMT="(a,a)",ADVANCE='NO') TRIM(cur_node%data), ' '        ! If so, then print it
          cur_node => cur_node%next                                          ! advance the cur_node
          if(ASSOCIATED(cur_node)) WRITE (*,FMT="(a)",ADVANCE='NO') '-> '    ! and print the -> for the next element
       ELSE
          WRITE (*,*)                                                        ! else, print the newline and return
          RETURN
       END IF
    END DO
  END SUBROUTINE dump

END PROGRAM pointers_linked_list


