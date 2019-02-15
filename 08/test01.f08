program main
  use, intrinsic :: iso_fortran_env, only: OUTPUT_UNIT
  use polynom
  use divisor
  use long_division

  implicit none

  real, dimension(4) :: a_c
  real, dimension(2) :: b_c
  type(polynom_t) :: a, b, c

  class(polynom_divisor_t), allocatable :: d
  allocate (long_division_t :: d)

  a_c = [-4, 0, -2, 1]
  b_c = [-3, 1]

  a = polynom_t (a_c)
  b = polynom_t (b_c)

  write (OUTPUT_UNIT, '(A)', advance='no') "a: "
  call a%write (OUTPUT_UNIT)
  write (OUTPUT_UNIT, '(A)', advance='no') "a: "
  call b%write (OUTPUT_UNIT)

  write (OUTPUT_UNIT, '(A)') "Add a + b:"
  c = a + b
  call c%write (OUTPUT_UNIT)

  write (OUTPUT_UNIT, '(A)') "Subtract a - b:"
  c = a - b
  call c%write (OUTPUT_UNIT)

  write (OUTPUT_UNIT, '(A)') "Multiply a * b:"
  c = a * b
  call c%write (OUTPUT_UNIT)

  write (OUTPUT_UNIT, '(A)') "Divide a / b:"
  call d%divide (a, b)
  call d%write (OUTPUT_UNIT)
end program main
