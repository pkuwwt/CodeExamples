!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  @file      format.f95
!  @Author    Mitch Richling<http://www.mitchr.me>
!  @Copyright Copyright 2012 by Mitch Richling.  All rights reserved.
!  @brief     embedded format statements.@EOL
!  @Keywords  none
!  @Std       F95

PROGRAM format

  ! One can embed a format statement into the WRITE as a literal
  WRITE (*,FMT="(1a,1f5.2)") 'The value is: ', 1.3

END PROGRAM format
