module long_division_ut
  use iso_fortran_env, only: r64 => REAL64, r32 => REAL32, i64 => INT64, OUTPUT_UNIT
  use unit_test, only: assert
  use polynom
  use long_division

  implicit none

  private

  public :: long_division_ut_divide_1, &
       long_division_ut_divide_2, &
       long_division_ut_divide_3, &
       long_division_ut_divide_4, &
       long_division_ut_divide_5, &
       long_division_ut_divide_6
contains
  subroutine long_division_ut_divide_1 (success)
    logical, intent(out) :: success
    type(polynom_t) :: a, b, q
    type(long_division_t) :: d
    call a%init ([1., 2., 3.])
    call b%init ([1., 2., 3.])
    call d%divide (a, b)
    q = d%quotient ()
    success = all (assert ([1.], q%get_coeff ()))
  end subroutine long_division_ut_divide_1

  subroutine long_division_ut_divide_2 (success)
    logical, intent(out) :: success
    type(polynom_t) :: a, b, q, r
    type(long_division_t) :: d
    call a%init ([1., 2., 3.])
    call b%init ([1., 2.])
    call d%divide (a, b)
    q = d%quotient ()
    r = d%remainder ()
    success = all (assert ([0.25, 1.5], q%get_coeff ()))
    success = success .and. all (assert ([0.75], r%get_coeff ()))
  end subroutine long_division_ut_divide_2

  subroutine long_division_ut_divide_3 (success)
    logical, intent(out) :: success
    type(polynom_t) :: a, b, q, r
    type(long_division_t) :: d
    call a%init ([1., 2., 3., 4., 5., 6., 7., 8., 9.])
    call b%init ([1., 3., 5.])
    call d%divide (a, b)
    q = d%quotient ()
    r = d%remainder ()
    a = b * q + r
    success = all (assert ([1., 2., 3., 4., 5., 6., 7., 8., 9.], a%get_coeff ()))
  end subroutine long_division_ut_divide_3

  subroutine long_division_ut_divide_4 (success)
    logical, intent(out) :: success
    type(polynom_t) :: a, b, q, r
    type(long_division_t) :: d
    call a%init ([1., 3., 5.])
    call b%init ([1., 2., 3., 4., 5., 6., 7., 8., 9.])
    call d%divide (a, b)
    q = d%quotient ()
    r = d%remainder ()
    success = all (assert ([0.], q%get_coeff ()))
    success = success .and. all (assert ([1., 3., 5.], r%get_coeff ()))
  end subroutine long_division_ut_divide_4

  subroutine long_division_ut_divide_5 (success)
    logical, intent(out) :: success
    type(polynom_t) :: a, b, q, r
    type(long_division_t) :: d
    call a%init ([1., 3., 5.])
    call b%init ([2.])
    call d%divide (a, b)
    q = d%quotient ()
    r = d%remainder ()
    success = all (assert ([0.5, 1.5, 2.5], q%get_coeff ()))
  end subroutine long_division_ut_divide_5

  subroutine long_division_ut_divide_6 (success)
    logical, intent(out) :: success
    type(polynom_t) :: a, b, q, r
    type(long_division_t) :: d
    call a%init ([1., 3., 5.])
    call b%init ([0.])
    call d%divide (a, b)
    q = d%quotient ()
    r = d%remainder ()
    success = all (assert ([0.], q%get_coeff ()))
    success = success .and. all (assert ([0.], r%get_coeff ()))
  end subroutine long_division_ut_divide_6

end module long_division_ut
