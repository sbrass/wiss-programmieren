program main
  use iso_fortran_env, only: i64 => INT64, r64 => REAL64
  use func_impl
  use integration

  implicit none

  real(r64), parameter :: h = 1e-4, a = 50
  integer(i64) :: n

  type(func3_t) :: f3
  type(func4_t) :: f4

  n = int ((1._r64 - (-1._r64)) / h, i64)
  !! Secure that n is even (for the Simpson rule).
  if (mod (n , 2_i64) /= 0) n = n + 1
  print *, "PV∫_-1^1 exp(x)/x"
  print *, "Cauchy-Hauptwertintegral mit ", n, " Punkten: ", cauchy_pv (f3, -1._r64, 1._r64, 0._r64, n)

  n = int (a / h, i64)
  !! Secure that n is even (for the Simpson rule).
  if (mod (n , 2_i64) /= 0) n = n + 1
  print *, "∫_0^∞ exp(-x)/√x = 2∫_-∞^∞ exp(-x^2)"
  print *, "Simpsonregel mit ", n, " Punkten: ", 2 * simpson_rule (f4, 0._r64, a, n)
end program main
