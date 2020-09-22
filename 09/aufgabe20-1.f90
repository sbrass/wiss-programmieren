program main
  use iso_fortran_env, only: ERROR_UNIT
  use list
  use cluster

  implicit none

  integer :: l, r
  real :: p

  type(grid_t) :: grid

  call parse_cmd (r, l, p)

  call grid%init (p, l)
  call grid%search_cluster_hk ()

  call grid%print_cluster ()
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
end program main
