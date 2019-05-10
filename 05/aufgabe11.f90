program main
  use iso_fortran_env, only: i64 => INT64, r64 => REAL64
  use hermite
  use derivatives

  implicit none

  integer(i64), dimension(4), parameter :: n = [1, 2, 5, 42]
  integer(i64), parameter :: steps = 1000_i64
  real(r64), parameter :: x_upper = 10._r64
  real(r64), parameter :: x_lower = -10._r64

  type(hermite_t) :: h

  integer :: i
   real(r64) :: x

  do i = 1, size(n)
     print *, "# n = ", n(i)
     call h%init (n(i))
     ! call h%write ()
     x = x_lower
     do while (x < x_upper)
        print *, x, h%evaluate (x)
        x = x + (x_upper - x_lower) / steps
     end do
  end do
end program main
