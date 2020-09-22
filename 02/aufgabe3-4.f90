program main
  use iso_fortran_env, only: i64 => INT64, r64 => REAL64
  use rng

  implicit none

  integer(i64) :: ri, rj
  integer(i64) :: a, c, m

  integer :: i

  rj = 1234
  a = 16807
  c = 0
  m = 2_i64**31 - 1

  do i = 1, 10000
     ri = lcg (rj, a, c, m)
     rj = lcg (ri, a, c, m)
     print *, abs(ri) / real(m, r64), abs(rj) / real(m, r64)
  end do
end program main
