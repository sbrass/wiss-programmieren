program main
  use iso_fortran_env, only: i64 => INT64, r64 => REAL64

  implicit none

  real(r64), parameter :: r_inf = 3.569946, &
       eps = 1e-5

  real(r64), dimension(4) :: r

  r(1) = bisect (1.5_r64, 2.5_r64, 0.5_r64, 0_i64)
  r(2) = bisect (3.0_r64, 3.3_r64, 0.5_r64, 1_i64)
  r(3) = bisect (3.3_r64, 3.5_r64, 0.5_r64, 2_i64)
  r(4) = bisect (3.5_r64, r_inf, 0.5_r64, 3_i64)

  print *, "Nullstellen: ", r
  print *, "Feigenbaum-Konstante: ", (r(3) - r(2)) / (r(4) - r(3))
contains
  function bisect (a, b, xi, n) result (r0)
    real(r64), intent(in) :: a, b, xi
    integer(i64), intent(in) :: n
    real(r64) :: r0
    real(r64) :: ra, rb, fra, fr0
    ra = a; rb = b
    r0 = (ra + rb) / 2
    do while (abs (ra - rb) > eps)
       fra = g_n (ra, xi, n)
       fr0 = g_n (r0, xi, n)
       if (fra * fr0 < 0) then
          rb = r0
       else
          ra = r0
       end if
       r0 = (ra + rb) / 2
    end do
  end function bisect

  function g_n (r, xi, n) result (g)
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
  end function g_n

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
