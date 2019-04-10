program main
  use rng

  implicit none

  integer :: ri, rj
  integer :: a, c, m

  integer :: i

  rj = 1234
  a = 137
  c = 187
  m = 256

  do i = 1, 128
     ri = lcg (rj, a, c, m)
     rj = lcg (ri, a, c, m)
     print *, ri / real(m), rj / real(m)
  end do
end program main
