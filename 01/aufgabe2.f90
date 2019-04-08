program main
  implicit none
  integer, allocatable :: seed(:)
  integer :: i, j, n

  real(8) :: r, prod

  ! character(15) :: str
  ! character(1) :: digit

  call random_seed(size = n)
  allocate(seed(n))
  seed = 12345
  call random_seed(put=seed)

  do j = 1, 1000
     prod = 1
     do i = 1, 100
        call random_number(r)
        prod = prod * r
     end do
     write (*, "(ES6.0)") prod * 10
  end do
end program main
