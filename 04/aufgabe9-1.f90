program main
  use iso_fortran_env, only: i64 => INT64, r64 => REAL64

  real(r64), parameter :: r_min = 0, r_inf = 3.569946
  integer(i64), parameter :: steps = 1000

  real(r64) :: r, delta

  r = r_min
  delta = (r_inf - r_min) / steps
  do while (r < r_inf)
     print *, r, fixpoint(r, 0.5_r64, 0_i64), &
          fixpoint(r, 0.5_r64, 1_i64), &
          fixpoint(r, 0.5_r64, 2_i64), &
          fixpoint(r, 0.5_r64, 3_i64)
     r = r + delta
  end do
contains
  function fixpoint (r, xi, n) result (g)
    real(r64), intent(in) :: r
    real(r64), intent(in) :: xi
    integer(i64), intent(in) :: n
    real(r64) :: g
    integer(i64) :: i
    g = logistic_map (xi, r)
    do i = 1, 2**n - 1
       g = logistic_map (g, r)
    end do
    g = 0.5 - g
  end function fixpoint

  !> Logistic map.
  !! \param xi element of [0,1].
  !! \param r  0 ≤ r < r_max.
  function logistic_map (xi, r) result (f)
    real(r64), intent(in) :: xi
    real(r64), intent(in) :: r
    real(r64) :: f
    f = r * xi * (1 - xi)
  end function logistic_map
end program main
