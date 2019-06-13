program main
  use iso_fortran_env, only: r64 => REAL64, i64 => INT64

  use beugung
  use integration, only: multi_simpson_rule

  implicit none

  real(r64), parameter :: a = 0.5_r64
  real(r64), parameter :: PI = 3.14159265358979323846_r64
  real(r64), dimension(2), parameter :: x_lower = [a, 0._r64], &
       x_upper = [2 * a, PI / 2]

  real(r64), dimension(2) :: q
  integer(i64) :: i, j
  type(beugung_t) :: func

  do i = -30_i64, 30_i64
     do j = -30_i64, 30_i64
        q(1) = 0.2_r64 * i / a
        q(2) = 0.2_r64 * j / a
        call func%set_q (q)
        print *, q, abs (multi_simpson_rule (func, x_lower, x_upper, 100_i64))**2
     end do
  end do
end program main
