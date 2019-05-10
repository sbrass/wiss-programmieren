program main
  use iso_fortran_env, only: i64 => INT64, r64 => REAL64
  use polynom
  use derivatives

  implicit none

  integer(i64), parameter :: steps = 1000
  real(r64), dimension(2), parameter :: hlog = [-16., 0.]

  type(polynom_func_t) :: f
  integer(i64) :: i
  real(r64) :: h

  call f%init (a = 0.5_r64, n = 3._r64)

  do i = 1, steps
     h = 10**((hlog(2) - hlog(1)) / steps * i + hlog(1))
     print *, h, naive_derive (f, 1._r64, h), symm_two_derive (f, 1._r64, h), symm_four_derive (f, 1._r64, h)
  end do
end program main
