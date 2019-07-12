program main
  use iso_fortran_env, only: r64 => REAL64, i64 => INT64
  use solver
  use harmonic

  implicit none

  class(func_t), dimension(:), allocatable :: ext_field
  real(r64), dimension(2) :: r, v
  type(newton_solver_t) :: newton_solver

  integer(i64) :: i

  allocate (harmonic_oscillator_t :: ext_field(2))
  select type (ext_field)
  type is (harmonic_oscillator_t)
     ext_field%mass = 1.
  end select

  r = [-1, +1]
  v = [0, 0]

  !! h = 10 / 10000 = 10-3
  call newton_solver%init (0._r64, 10._r64, 10000_i64)
  print *, r, v, 1.
  do i = 1, 10000
     call newton_solver%solve_step (ext_field, r, v)
     print *, r, v, 0.5 * (dot_product (v, v) + dot_product (r, r))
     !! F = -∇φ ⇒ φ = 1/2 r²
     !! T + V = const. ⇒ 1/2 m v² + 1/2 r²
  end do
end program main
