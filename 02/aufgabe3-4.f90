program main
  use rng

  implicit none

  integer :: ri, rj
  integer :: a, c, m

  integer :: i

  rj = 1234
  a = 16807
  c = 0
  m = 2**31 - 1

  do i = 1, 10000
     ri = lcg (rj, a, c, m)
     rj = lcg (ri, a, c, m)
     print *, abs(ri) / real(m), abs(rj) / real(m)
  end do
end program main
