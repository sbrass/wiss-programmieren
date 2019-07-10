program main
  use iso_fortran_env, only: OUTPUT_UNIT, r64 => REAL64, i64 => INT64

  implicit none

  real(r64), parameter :: pi = 3.141592653589793

  write (OUTPUT_UNIT, *) compute_elliptic (gaussian, 2._r64, 1._r64, 100000_i64)
  ! write (OUTPUT_UNIT, *) compute_elliptic (one, 2._r64, 1._r64, 1000_i64), pi * 2
contains
  function compute_elliptic (func, a, b, n) result (int)
    interface
       function func (x) result (f)
         import :: r64
         real(r64), dimension(2), intent(in) :: x
         real(r64) :: f
       end function func
    end interface
    real(r64), intent(in) :: a, b
    integer(i64), intent(in) :: n
    real(r64) :: int
    real(r64), dimension(2) :: x, r
    integer(i64) :: i
    int = 0.
    do i = 1, n
       call random_number (r)
       !! Abbilden auf Ellipse-einschließendes Rechteck.
       !! [0, 1]² → [-a, a] x [-b, b],
       !! x = (2 * a) * ux - a,
       !! y = (2 * b) * uy - b.
       x(1) = 2 * a * r(1) - a
       x(2) = 2 * b * r(2) - b
       if ((x(1) / a )**2 + (x(2) / b)**2 <= 1) then
          int = int + func (x)
       end if
    end do
    int = 4 * a * b * int / n
  end function compute_elliptic

  ! function one (x) result (f)
  !   real(r64), dimension(2), intent(in) :: x
  !   real(r64) :: f
  !   f = dot_product(x, x)
  !   f = 1
  ! end function one

  function gaussian (x) result (f)
    real(r64), dimension(2), intent(in) :: x
    real(r64) :: f
    f = exp(-x(1)**2)
  end function gaussian
end program main
