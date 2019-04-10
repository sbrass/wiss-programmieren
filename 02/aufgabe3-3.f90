program main
  use rng

  implicit none

  integer :: ri, rj
  integer :: a, c, m

  integer :: i

  rj = 123456789
  a = 65539
  c = 0
  m = 2**31

  do i = 1, 10000
     ri = lcg (rj, a, c, m)
     rj = lcg (ri, a, c, m)
     print *, ri / real(m), rj / real(m)
  end do
end program main
