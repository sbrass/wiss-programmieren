program main
  use rng

  implicit none

  integer :: ri
  integer :: a1, a2, a3
  integer :: i

  real :: x, y

  ri = 1234
  a1 = 13
  a2 = 17
  a3 = 5

  do i = 1, 10000
     ri = xorshift (ri, a1, a2, a3)
     !! Inversion method.
     y = ri / real (2**30)
     x = y**(1. / 3)
     print *, x
  end do
end program main
