program main
  use iso_fortran_env, only: r64 => REAL64, i64 => INT64

  use elektrostatik
  use integration, only: multi_simpson_rule

  implicit none

  real(r64), parameter :: PI = 3.14159265358979323846_r64
  !! Integration limits are chosen such that |x| < a, |y| < a, |z| < a.
  !! → |x/a| < 1, |y/a| < 1, |z/a| < 1.
  real(r64), dimension(3), parameter :: x_lower = [-1, -1, -1], &
       x_upper = [1, 1, 1]

  real(r64) :: x, monopol, dipol
  integer(i64) :: i
  type(potential_static_t) :: phi_static
  type(potential_dynamic_t) :: phi_dynamic

  do i = 1_i64, 80_i64
     x = 0.1_r64 * i !! x / a
     call phi_static%set_x (x)
     call phi_dynamic%set_x (x)
     !! Monopol only for static potential.
     monopol = 8. / x
     !! Dipol only for dynamic potential.
     dipol = 8. / 3. / x**2
     print *, x, multi_simpson_rule (phi_static, x_lower, x_upper, 100_i64), monopol, &
          multi_simpson_rule (phi_dynamic, x_lower, x_upper, 100_i64), dipol
  end do
end program main
