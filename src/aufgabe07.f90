program main
  !! Count, mean and variance.
  integer, dimension(M) :: cnt
  real, dimension(2, M) :: n_i
  real :: chi2
  !! Initialize mean, variance and chi2.
  cnt = 0
  n_i = 0.
  do it = 1, 15
     !! Histogram N random numbers in M bins.
     !! ...
     do k = 1, M
        call update_mean_variance (cnt(k), n_i(:, k), n_hist(k))
     end do
     !! Compute chi-squared for it > 1.
  end do
  do k = 1, M
     !! Print mean and variance.
     print *, n_i(1, k), compute_variance (cnt(k), n_i(:, k))
  end do
contains
  !! Welford's algorithm for variance.
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
