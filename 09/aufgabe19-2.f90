program main
  use iso_fortran_env, only: ERROR_UNIT
  use list
  use cluster

  implicit none

  integer :: l, r
  real :: p

  type(grid_t) :: grid
  integer :: i, n_percolation
  real :: m_inf

  call parse_cmd (r, l, p)

  n_percolation = 0
  m_inf = 0
  do i = 1, r
     call grid%init (p, l)
     call grid%search_cluster ()
     if (grid%is_percolated ()) n_percolation = n_percolation + 1
     m_inf = update_mean (i, m_inf, grid%get_ratio_max_cluster ())
  end do
  print *, p, l, r, real (n_percolation) / real (r), m_inf
contains
  subroutine parse_cmd (r, l, p)
    integer, intent(out) :: r, l
    real, intent(out) :: p
    character(len=100) :: buf
    if (command_argument_count () /= 3) then
       write (*, *)  "Provide: [REALISATION] [GRID SIZE] [PROBABILITY]"
       stop
    end if
    call get_command_argument (1, buf)
    read (buf, *) r
    call get_command_argument (2, buf)
    read (buf, *) l
    call get_command_argument (3, buf)
    read (buf, *) p
  end subroutine parse_cmd

  real function update_mean (n, mean, val) result (nmean)
    integer, intent(in) :: n
    real, intent(in) :: mean
    real, intent(in) :: val
    nmean = mean + (val - mean) / n
  end function update_mean
end program main
