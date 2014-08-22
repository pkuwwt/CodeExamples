CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  @file      chars.f
C  @Author    Mitch Richling<http://www.mitchr.me>
C  @Copyright Copyright 1993 by Mitch Richling.  All rights reserved.
C  @Revision  $Revision$ 
C  @SCMdate   $Date$
C  @brief     Character (string) handeling facilities of Fortran 77.@EOL
C  @Keywords  formated i/o screen stdin stdout FORTRAN 77
C  @Std       F77
C

       program chars

C      Character types should have a specified width (10 in this case).       
C      The length may be determined automatically for constants.
       character chs1*10
       character chs2*10

C      An ARRAY of characters (2 elements, each 12 chars long) may be
C      declared like so:
       character charr(10)*22

C      Strings may be assigned.
       chs1     = '0123456789'
       charr(1) = 'firstOne'
       charr(2) = 'secondOne'

C      A substring may be assigned too (right padded if needed)
       chs1(2:4) = 'BCD'

C      If the value is shorter than the character variable, then
C      the value is padded with spaces on the right to the correct size.
       chs2 = '123456789'

       write (*,*) 'x', chs1, 'x'
       write (*,*) 'x', chs2, 'x'

C      FORTRAN has a handy substring notation:
       write (*,*) 'x', chs1(2:4), 'x'
       write (*,*) 'x', chs1(:4),  'x'
       write (*,*) 'x', chs1(2:),  'x'

C      The order of the subscripts is important!  This is the 2nd to the
C      4th chars of array element 1:
       write (*,*) 'x', charr(1)(2:4), 'x'

C      The // operator is the only operator FORTRAN 77 has for strings.
C      Note the space is because chs2 ENDS with a space. :)
       write (*,*) 'x', (chs2 // chs1), 'x'

C      Characters may be compared.
       if ( .not. ('abc' .eq. 'ABC')) then
          write (*,*) 'abc .eq. ABC is FALSE'
       end if

C      FORTRAN only specifies that A<B<..., 0<1<2..., all letters are
C      before (or after) all numbers, and the space is before
C      everything.  For example case may be folded.
       if ('DEF' .gt. 'ABC') then
          write (*,*) 'DEF .gt. ABC is TRUE'
       end if

C      FORTRAN 77 has four functions that use the ASCII collating sequence
C      to determine order of strings.  They are LGE, LGT, LLE, LLT.
       if (LGE('abc', 'ABC')) then
          write (*,*) 'LGE(abc, ABC) is TRUE'
       end if

C      ICHAR converts the first char of a string into a number
       write (*,*) 'ichar(a)=', ichar('a'), ' ichar(A)=', ichar('B')

C      CHAR converts an integer into a char
       write (*,*) 'char(97)=', char(97), ' char(66)=', char(66)

C      You can search with index:
       write (*,*) 'bar is at ', index('foobar', 'bar'), ' in foobar'

C      The length of a string can be found with LEN.  Note, that this is
C      the allocated space, not just the non-white space.
       write (*,*) 'The length of chs1=', len(chs1)
       write (*,*) 'The length of chs2=', len(chs2)

C      Now we pass a string to a subroutine
       call excsub(chs1)

       end

      subroutine excsub(c)
C     This is how you declare a string without specifying the size
C     explicitly -- the length is passed with the string!  This is 
C     natural because the *X notation changes the size of a TYPE.
        character*(*) c
        write (*,*) 'In excsub(): x', c, 'x'
        return
      end
