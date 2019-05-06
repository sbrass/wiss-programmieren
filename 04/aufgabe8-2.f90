program main
  use iso_fortran_env, only: i64 => INT64, r64 => REAL64

  implicit none

  real(r64), parameter :: r_min = 1.6, &
       r_max = 2.5
  integer(i64), parameter :: steps = 1000, &
       transient = 100, &
       steady = 300

  real(r64) :: r, delta
  real(r64), dimension(steady) :: xn

  r = r_min
  delta = (r_max - r_min) / steps
  do while (r < r_max)
     call fill_xn (r, transient, steady, xn)
     print *, r, xn
     r = r + delta
  end do
contains
  subroutine fill_xn (r, transient, steady, xn)
    real(r64), intent(in) :: r
    integer(i64), intent(in) :: transient
    integer(i64), intent(in) :: steady
    real(r64), dimension(steady) :: xn
    real(r64) :: x
    integer(i64) :: i
    call random_number (x)
    !! Warmup.
    do i = 1, transient
       x = cubic_map (x, r)
    end do
    !! Fillup.
    do i = 1, steady
       x = cubic_map (x, r)
       xn(i) = x
    end do
  end subroutine fill_xn

  !> Logistic map.
  !! \param xi element of [0,1].
  !! \param r  0 ≤ r < r_max.
  function cubic_map (xi, r) result (x)
    real(r64), intent(in) :: xi
    real(r64), intent(in) :: r
    real(r64) :: x
    x = r * xi - xi**3
  end function cubic_map
end program main
