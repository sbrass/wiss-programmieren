program main
  use rng

  implicit none

  real, parameter :: PI = 3.1415926

  integer :: ri, rj
  integer :: a1, a2, a3
  integer :: i

  real, dimension(2) :: u, r

  rj = 1234
  a1 = 13
  a2 = 17
  a3 = 5

  do i = 1, 10000
     !! Generate two random numbers.
     ri = xorshift (rj, a1, a2, a3)
     rj = xorshift (ri, a1, a2, a3)
     !! Map the two random numbers to the interval [0, 1].
     u(1) = ri / real (2**30)
     u(2) = rj / real (2**30)
     !! Generate normal-distributed random numbers.
     r = box_muller (u)
     print *, r(1), r(2)
  end do
contains
  function box_muller (ui) result (y)
    real, dimension(2), intent(in) :: ui
    real, dimension(2) :: y
    y(1) = sqrt(-2. * log (ui(1))) * cos (2. * PI * ui(2))
    y(2) = sqrt(-2. * log (ui(1))) * sin (2. * PI * ui(2))
  end function box_muller
end program main
