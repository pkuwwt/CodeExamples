!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  @file      no_advance_print.f95
!  @Author    Mitch Richling<http://www.mitchr.me>
!  @Copyright Copyright 2012 by Mitch Richling.  All rights reserved.
!  @brief     Finally!  We can print without a newline!@EOL
!  @Std       F95
!
!             For another example, see 'pointers_linked_list.f95'

PROGRAM no_advance_print

  ! Non-advancing I/O now has a standard solution.  Note that the FMT is required if ADVANCE is used.
  WRITE (*,FMT="(1a)",ADVANCE='NO') 'The value is: '
  WRITE (*,FMT="(1f5.2)") 1.3

END PROGRAM no_advance_print
