program main
  use iso_fortran_env, only: i64 => INT64, r64 => REAL64
  use func_impl
  use integration

  implicit none

  real(r64), parameter :: h = 1e-4, eps = 1e-5
  integer(i64) :: n

  type(func1_t) :: f1
  type(func2_t) :: f2

  n = int ((100._r64 - 1._r64) / h, i64)
  !! Secure that n is even (for the Simpson rule).
  if (mod (n , 2_i64) /= 0) n = n + 1
  print *, "∫_1^100 exp(-x)/x"
  print *, "Trapezregel mit ", n, " Punkten: ", trapez_rule (f1, 1._r64, 100._r64, n)
  print *, "Simpsonregel mit ", n, " Punkten: ", simpson_rule (f1, 1._r64, 100._r64, n)

  n = int ((1._r64 - eps) / h, i64)
  if (mod (n , 2_i64) /= 0) n = n + 1
  print *, "∫_0^1 x sin(1/x)"
  print *, "Trapezregel mit ", n, " Punkten: ", trapez_rule (f2, eps, 1._r64, n)
  print *, "Mittelpunktregel mit ", n, " Punkten: ", midpoint_rule (f2, eps, 1._r64, n)
  print *, "Simpsonregel mit ", n, " Punkten: ", simpson_rule (f2, eps, 1._r64, n)
end program main
