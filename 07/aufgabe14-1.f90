program main
  use iso_fortran_env, only: r64 => REAL64, i64 => INT64

  use beugung
  use integration, only: multi_simpson_rule

  implicit none

  real(r64), parameter :: a = 0.5_r64
  real(r64), parameter :: PI = 3.14159265358979323846_r64
  real(r64), dimension(2), parameter :: x_lower = [a, 0._r64], &
       x_upper = [2 * a, 2 * PI]

  real(r64) :: q, u
  integer(i64) :: i
  type(beugung_t) :: func

  do i = 1_i64, 50_i64
     q = 0.1_r64 * i / a
     call func%set_q (q)
     u = 2._r64 * PI / q * (2._r64 * a * bessel_jn (1, 2._r64 * a * q) - a * bessel_jn (1, a * q))
     print *, q, abs (multi_simpson_rule (func, x_lower, x_upper, 1000_i64))**2, abs (u)**2
  end do
end program main
