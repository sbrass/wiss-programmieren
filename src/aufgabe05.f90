program main
  implicit none

  integer, parameter :: M = 100, N = 1000
  integer, dimension(M) :: cnt
  real, dimension(2, M) :: n_i

  do i = 1, N
     ! …
     k = floor (r * M) !! Histogram random number r of [0, 1] into bin k.
     call add_to_bin_k (cnt, n_i, k)
  end do
  print *, n_i(1, :), n_i(2, :) / (cnt(:) - 1)
contains
  ! Welford's algorithm for variance.
  subroutine add_to_bin_k (cnt, n_i, i)
    integer, dimension(:), intent(in) :: cnt !! short for count
    real, dimension(:, :), intent(inout) :: n_i
    integer, intent(in) :: i
    real :: delta
    cnt(i) = cnt(i) + 1
    !! Add one new random number to bin.
    delta = 1. - n_i(1, i)
    n_i(1, i) = n_i(1, i) + delta / cnt(i)
    n_i(2, i) = delta * (1. - n_i(1, i))
  end subroutine add_to_bin_k
end program main
