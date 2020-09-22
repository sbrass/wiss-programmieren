program main
  implicit none

  integer, dimension(12) :: label = [1, 2, 3, 4, 5, 5, 7, 5, 9, 3, 3, 12]

  call union (7, 4, label)
  call union (5, 4, label)
  call union (5, 3, label)
  print *, label
  call reduce (label)
  print *, label
contains
  function find (x, label) result (y)
    integer, intent(in) :: x
    integer, dimension(:), intent(inout) :: label
    integer :: y, xx, z
    y = x
    do while (label(y) /= y)
       y = label(y)
    end do
  end function find

  subroutine union (left, above, label)
    integer, intent(in) :: left
    integer, intent(in) :: above
    integer, dimension(:), intent(inout) :: label
    label(find (left, label)) = find (above, label)
  end subroutine union

  subroutine reduce (label)
    integer, dimension(:), intent(inout) :: label
    integer :: i
    do i = size (label), 1
       print *, i, "|", label
       label(i) = find(label(i), label)
    end do
  end subroutine reduce
end program main
