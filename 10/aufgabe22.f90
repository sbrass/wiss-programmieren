program main
  use iso_fortran_env, only: r64 => REAL64, i64 => INT64, OUTPUT_UNIT
  use importance

  implicit none

  integer(i64) :: k_max
  real(r64) :: beta

  integer(i64) :: i, n_samples

  call parse_cmd (k_max, beta)

  do i = 1, k_max
     n_samples = 10**i
     print *, n_samples, simple_sampling (func, beta, 0._r64, 100._r64, n_samples), &
          importance_sampling (func, beta, n_samples), &
          markov_sampling (func, beta, 0._r64, 100._r64, n_samples)
  end do
contains
  function func (xi, beta) result (f)
    real(r64), intent(in) :: xi
    real(r64), intent(in) :: beta
    real(r64) :: f
    f = exp (-beta * xi) * xi**2
  end function func

  subroutine parse_cmd (k_max, beta)
    integer(i64), intent(out) :: k_max
    real(r64), intent(out) :: beta
    character(len=100) :: buf
    if (command_argument_count () /= 2) then
       write (*, *)  "Provide: [K_MAX] [BETA]"
       stop
    end if
    call get_command_argument (1, buf)
    read (buf, *) k_max
    call get_command_argument (2, buf)
    read (buf, *) beta
  end subroutine parse_cmd
end program main
