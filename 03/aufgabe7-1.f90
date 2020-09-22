program main
  use iso_fortran_env, only: i64 => INT64, r64 => REAL64
  use rng

  implicit none

  integer, parameter :: M = 1000, N = 100000

  !! Random number generator.
  integer(i64) :: ri
  integer(i64) :: a, c, md
  integer :: it, i, k

  !! Histogram.
  integer, dimension(M) :: n_hist

  !! Mean and variance.
  integer, dimension(M) :: cnt
  real, dimension(2, M) :: n_i
  real :: chi2

  ri = 123456789
  a = 65539
  c = 0
  md = 2**31

  !! Initialize mean, variance and chi2.
  cnt = 0
  n_i = 0.
  print *, "||--------------------------------------------------||"
  do it = 1, 15
     !! Reset histogram.
     n_hist = 0
     do i = 1, N
        ri = lcg (ri, a, c, md)
        !! Histogram random number r of [0, 1] into bin k.
        k = floor (ri / real (md, r64) * M) + 1
        n_hist(k) = n_hist(k) + 1
     end do
     do k = 1, M
       call update_mean_variance (cnt(k), n_i(:, k), n_hist(k))
     end do
     chi2 = 0
     if (it >= 2) then
        do k = 1, M
           chi2 = chi2 + (n_hist(k) - n_i(1, k))**2 / n_i(1, k)
        end do
     end if
     print *, "It.: ", it, ", χ² = ", chi2
  end do


  print *, "||--------------------------------------------------||"
  print *, "<nᵢ>, Var(nᵢ)"
  do k = 1, M
     print *, n_i(1, k), compute_variance (cnt(k), n_i(:, k))
  end do
  print *, "||--------------------------------------------------||"
contains
  ! Welford's algorithm for variance.
  subroutine update_mean_variance (cnt, mean_var, val)
    integer, intent(inout) :: cnt !! short for count
    real, dimension(2), intent(inout) :: mean_var
    integer, intent(in) :: val
    real :: delta
    cnt = cnt + 1
    delta = val - mean_var(1)
    mean_var(1) = mean_var(1) + delta / cnt
    mean_var(2) = delta * (val - mean_var(1))
  end subroutine update_mean_variance

  function compute_variance (cnt, mean_var) result (var)
    integer, intent(in) :: cnt
    real, dimension(2), intent(in) :: mean_var
    real :: var
    if (cnt > 1) then
       var = mean_var(2) / cnt
    else
       var = 0
    end if
  end function compute_variance
end program main
