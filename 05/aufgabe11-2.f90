program main
  use iso_fortran_env, only: i64 => INT64, r64 => REAL64
  use hermite
  use derivatives

  implicit none

  integer(i64), dimension(3), parameter :: n = [1, 2, 5]
  integer(i64), parameter :: steps = 1000_i64
  real(r64), parameter :: x_upper = 2._r64
  real(r64), parameter :: x_lower = -2._r64
  real(r64), parameter :: h = 1e-4

  type(hermite_t) :: hermite

  integer :: i
  real(r64) :: x, dgl

  do i = 1, size(n)
     print *, "# n = ", n(i)
     call hermite%init (n(i))
     x = x_lower
     do while (x < x_upper)
        dgl = symm_two_derive_snd (hermite, x, h) &
             - 2._r64 * x * symm_two_derive (hermite, x, h ) &
             + 2._r64 * n(i) * hermite%evaluate (x)
        print *, x, (abs(dgl) < h)
        x = x + (x_upper - x_lower) / steps
     end do
  end do
end program main
