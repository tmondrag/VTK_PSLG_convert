! Module for setting float precision and integer width program-wide
! Use the variables defined in ISO_FORTRAN_ENV, default compiler kinds, or
! custom kinds
! Intrinsic select_int_kind(r) should return the shortest integer kind that can
! accomodate an integer range from -10^r to 10^r
! Intrinsic select_real_kind(p,r) should return the shortest real kind that can
! accomodate a real number with decimal precision of at least p digits and an
! exponent range from 10^-r to 10^r. it is not necessary to specify r.

MODULE kindprecision
  USE ISO_FORTRAN_ENV
  IMPLICIT NONE
  INTEGER, PARAMETER                    :: default_int = kind(1)
  INTEGER, PARAMETER                    :: default_float = kind(1.0e0)
  INTEGER, PARAMETER                    :: default_double = kind(1.0d0)
  INTEGER, PARAMETER                    :: IP = default_int
  INTEGER, PARAMETER                    :: LI = selected_int_kind(10)
  INTEGER, PARAMETER                    :: SI = selected_int_kind(4)
  INTEGER, PARAMETER                    :: SP = default_float
  INTEGER, PARAMETER                    :: DP = default_double
  INTEGER, PARAMETER                    :: StrBuffLen = 256

CONTAINS

  SUBROUTINE print_precision_and_range_external(fd)
    IMPLICIT NONE
    INTEGER(kind=IP),INTENT(IN)         :: fd   ! Unit number/file descriptor
    INTEGER(kind=default_int)           :: aa
    INTEGER(kind=IP)                    :: ab
    INTEGER(kind=SI)                    :: ac
    INTEGER(kind=LI)                    :: ad
    REAL(kind=SP)                       :: fa
    REAL(kind=DP)                       :: da

    WRITE(fd,*) "Range of default integer kind is ",-HUGE(aa)-1," to ",HUGE(aa)
    WRITE(fd,*) "Range of selected integer kind is ",-HUGE(ab)-1," to ",HUGE(ab)
    WRITE(fd,*) "Range of selected short integer kind is ",-HUGE(ac)-1," to ",HUGE(ac)
    WRITE(fd,*) "Range of selected long integer kind is ",-HUGE(ad)-1," to ",HUGE(ad)
    WRITE(fd,'(A,I3,A,I5,A)') " Default single precision kind has ",PRECISION(1.0e0),&
                              " decimal digits of precision and an exponent range of +/-",&
                              RANGE(1.0e0)
    WRITE(fd,'(A,I3,A,I5,A)') " Selected single precision kind has ",PRECISION(fa),&
                              " decimal digits of precision and an exponent range of +/-",&
                              RANGE(fa)
    WRITE(fd,*) "The smallest single precision number is ",TINY(fa)
    WRITE(fd,*) "The largest single precision number is ",HUGE(fa)
    WRITE(fd,'(A,I3,A,I5,A)') " Default double precision kind has ",PRECISION(1.0d0),&
                              " decimal digits of precision and an exponent range of +/-",&
                              RANGE(1.0d0)
    WRITE(fd,'(A,I3,A,I5,A)') " Selected double precision kind has ",PRECISION(da),&
                              " decimal digits of precision and an exponent range of +/-",&
                              RANGE(da)
    WRITE(fd,*) "The smallest double precision number is ",TINY(da)
    WRITE(fd,*) "The largest double precision number is ",HUGE(da)
  END SUBROUTINE print_precision_and_range_external
END MODULE kindprecision
