program main
  use rng
  use iso_fortran_env, only: i64 => INT64, r64 => REAL64

  implicit none

  integer(i64) :: ri, rj
  integer(i64) :: a1, a2, a3

  integer :: i

  rj = 1234

  a1 = 13
  a2 = 17
  a3 = 5

  do i = 1, 1000
     ri = xorshift64 (rj, a1, a2, a3)
     rj = xorshift64 (ri, a1, a2, a3)
     print *, ri / real (2_i64**62, r64), rj  / real (2_i64**62, r64)
  end do
end program main
