program main
  use rng

  implicit none

  !! Zwölfer-Regel
  integer, parameter :: N = 12

  integer :: ri
  integer :: a1, a2, a3
  integer :: i, j

  real :: y

  ri = 1234
  a1 = 13
  a2 = 17
  a3 = 5

  do i = 1, 10000
     y = - N / 2.
     do j = 1, N
        ri = xorshift (ri, a1, a2, a3)
        y = y + ri / real (2**30)
     end do
     !! Z = μ + σ * y, change standard deviation to 1.
     print *, y
  end do
end program main
