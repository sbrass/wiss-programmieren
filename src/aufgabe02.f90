program main
  implicit none

  integer, allocatable :: seed(:)
  integer :: n
  real :: x

  call random_seed (size = n)
  allocate (seed(n))
  seed = 12345
  call random_seed (put = seed)

  ! Aufgabenlösung

  ! Zufallszahl erzeugen.
  call random_number (x)
end program main
