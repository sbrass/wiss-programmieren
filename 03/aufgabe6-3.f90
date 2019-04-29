program main
  use rng

  implicit none

  real, parameter :: PI = 3.1415926

  integer :: ri
  integer :: a1, a2, a3
  integer :: i

  real :: x, y

  ri = 1234
  a1 = 13
  a2 = 17
  a3 = 5

  do i = 1, 10000
     do
        !! (i) Draw a uniformly-distributed random number x from [0, 1] and map
        !! to the interval [0, π].
        ri = xorshift (ri, a1, a2, a3)
        x = PI * ri / real (2**30)
        !! (ii) Draw uniformly-distributed random number y from [0, 1] and map
        !! to the interval [0, 0.5].
        ri = xorshift (ri, a1, a2, a3)
        y = 0.5 * ri / real (2**30)
        !! Accept.
        if (y <= 0.5 * sin (x)) exit
     end do
     print *, x
  end do
end program main
