program main
  implicit none

  integer :: ri, rj
  integer :: a, c, m

  integer :: i

  rj = 1234
  a = 20
  c = 120
  m = 6075

  do i = 1, 1000
     ri = mod (a * rj + c, m)
     rj = mod (a * ri + c, m)
     print *, ri / real(m), rj / real(m)
  end do
end program main
