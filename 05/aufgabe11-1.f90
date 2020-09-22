program main
  use iso_fortran_env, only: i64 => INT64, r64 => REAL64
  use hermite
  use derivatives

  implicit none

  real(r64), parameter :: PI = 3.141592653589793
  integer(i64), dimension(4), parameter :: n = [1, 2, 5, 42]
  integer(i64), parameter :: steps = 1000_i64
  real(r64), parameter :: x_upper = 2._r64
  real(r64), parameter :: x_lower = -2._r64

  type(hermite_t) :: h

  integer :: i
  real(r64) :: x

  do i = 1, size(n)
     print *, "# n = ", n(i)
     call h%init (n(i))
     ! call h%write ()
     x = x_lower
     do while (x < x_upper)
        print *, x, h%evaluate (x), wavefunction(x, n(i), h)
        x = x + (x_upper - x_lower) / steps
     end do
  end do
contains
  real(r64) function wavefunction (xi, ni, hermite) result (psi)
    real(r64), intent(in) :: xi
    integer(i64), intent(in) :: ni
    type(hermite_t), intent(in) :: hermite
    psi = hermite%evaluate (xi)
    psi = psi * exp (-0.5 * xi**2)
    psi = psi / sqrt (2._r64**ni * faculty(ni) * sqrt (PI))
  end function wavefunction

  recursive integer(i64) function faculty (n) result (f)
    integer(i64), intent(in) :: n
    if (n <= 0) then
       f = 1
       return
    end if
    f = faculty (n - 1) * n
  end function faculty
end program main
