program main
  use rng

  implicit none

  integer :: ri, rj
  integer :: a1, a2, a3

  integer :: i

  rj = 1234

  a1 = 13
  a2 = 17
  a3 = 5

  do i = 1, 10000
     ri = xorshift (rj, a1, a2, a3)
     rj = xorshift (ri, a1, a2, a3)
     print *, ri / real (2**30), rj  / real (2**30)
  end do
end program main
