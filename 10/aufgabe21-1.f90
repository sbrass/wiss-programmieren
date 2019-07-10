program main
  use iso_fortran_env, only: OUTPUT_UNIT, r64 => REAL64, i64 => INT64

  implicit none

  real(r64), parameter :: pi = 3.141592653589793

  write (OUTPUT_UNIT, *) 10, abs (compute_pi (10_i64)- pi)
  write (OUTPUT_UNIT, *) 100, abs (compute_pi (100_i64)- pi)
  write (OUTPUT_UNIT, *) 1000, abs (compute_pi (1000_i64)- pi)
  write (OUTPUT_UNIT, *) 10000, abs (compute_pi (10000_i64)- pi)
  write (OUTPUT_UNIT, *) 100000, abs (compute_pi (100000_i64)- pi)
  write (OUTPUT_UNIT, *) 1000000, abs (compute_pi (1000000_i64)- pi)
contains
  !! ∫_{|r| ≤ 1} d²r → f(x, y) = 1.
  !! Kreis mit Radius 1 und Mittelpunkt in (0, 0).
  function compute_pi (n) result (pi)
    integer(i64), intent(in) :: n
    real(r64) :: pi
    real(r64), dimension(2) :: x
    integer(i64) :: i
    pi = 0.
    do i = 1, n
       call random_number (x)
       if (dot_product(x, x) <= 1.) then
          pi = pi + 1.
       end if
    end do
    !! Bisher: Bestimmung der Viertelfläche eines Kreises.
    pi = 4 * pi / n
  end function compute_pi
end program main
