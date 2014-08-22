! -*- Mode:F90; Coding:us-ascii-unix; fill-column:132 -*-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!  @file      f90Rand.f90
!  @Author    Mitch Richling<http://www.mitchr.me/>
!  @Copyright Copyright 1993 by Mitch Richling.  All rights reserved.
!  @breif     Example of the Fortran 90 random number generateor@EOL
!  @Keywords  random number Fortran 90 example
!  @Std       F90
!
!  One of the new features of Fortran 90 was a standard random number generator.  The standard doesn't specify the algorithm used to
!  generate the numbers.  As one might expect, many implementations have quite bad generators that should never be used for serious
!  work.  The lack of standardization also means that one can not expect programs to have reproducible results on different
!  platforms. Still, the facilities are adequate for some non-critical applications, and they are always available.  On most
!  platforms the generator is seeded with the same value upon start up, so one will always get the same sequence from run to run.
!  We are lucky that Fortran 90 also came with a date_and_time() function that can be used to seed the generator.  Still, the system
!  clock may not have enough resolution to always render a different seed for different runs, so don't trust this method completely.
!             

!-----------------------------------------------------------------------------------------------------------------------------------
program f90Rand

  implicit none

  real                               :: rndReal
  real, dimension(5)                :: rndRealArr
  integer                            :: seedSize
  integer, dimension(:), allocatable :: seed
  integer, dimension(8)              :: dtVals

  call DATE_AND_TIME(VALUES=dtVals)
  write (*,*) 'Clock Values=', dtVals

  ! The seed is generally 4 or 8 integers long.  We find out now.
  call RANDOM_SEED(SIZE=seedSize)
  write (*,*) 'Seed Size: ', seedSize

  ! We intend to use the DATE_AND_TIME value to initialize the seed if it's bit enough
  if(seedSize .gt. 8) then
     write (*,*) 'ERROR: Seed size too large to init with DATE_AND_TIME return'
     stop
  end if

  ! We know the seed size, so allocate space for it and query to see what it is
  allocate(seed(seedSize)) 
  call RANDOM_SEED(GET=seed)
  write (*,*) 'Old Seed: ', seed

  ! Use the last bits of the DATE_AND_TIME values array to seed the number
  call RANDOM_SEED(PUT=dtVals((9-seedSize):8))

  ! Re-query the seed to make sure it worked.
  call RANDOM_SEED(GET=seed)
  write (*,*) 'New Seed: ', seed

  ! Get one random number
  rndReal = 0
  call RANDOM_NUMBER(rndReal)
  write (*,*) 'Random Number: ', rndReal

  ! Get an array of random numbers
  rndRealArr = 0
  call RANDOM_NUMBER(rndRealArr)
  write (*,*) 'Random Array: ', rndRealArr

end program f90Rand

