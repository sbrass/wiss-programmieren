module polynom_ut
  use iso_fortran_env, only: r64 => REAL64, r32 => REAL32, i64 => INT64, OUTPUT_UNIT
  use unittest
  use polynom

  implicit none

  private

  public :: polynom_ut_init_coefficients, &
       polynom_ut_init_single_coefficient, &
       polynom_ut_operator_add_1, &
       polynom_ut_operator_add_2, &
       polynom_ut_operator_add_3, &
       polynom_ut_operator_subtract_1, &
       polynom_ut_operator_subtract_2, &
       polynom_ut_operator_subtract_3, &
       polynom_ut_operator_multiply_1, &
       polynom_ut_operator_multiply_2, &
       polynom_ut_reduce_1, &
       polynom_ut_reduce_2
contains
  subroutine polynom_ut_init_coefficients (success)
    logical, intent(out) :: success
    type(polynom_t) :: p
    call p%init ([1., 2., 3.])
    success = all (assert ([1., 2., 3.], p%get_coeff ()))
    success = success .and. assert (2, p%deg ())
    success = success .and. assert (3._r32, p%lc ())
  end subroutine polynom_ut_init_coefficients

  subroutine polynom_ut_init_single_coefficient (success)
    logical, intent(out) :: success
    type(polynom_t) :: p
    call p%init (4, 1.)
    success = all (assert ([0., 0., 0., 0., 1.], p%get_coeff ()))
    success = success .and. assert (4, p%deg ())
    success = success .and. assert (1., p%lc ())
  end subroutine polynom_ut_init_single_coefficient

  subroutine polynom_ut_operator_add_1 (success)
    logical, intent(out) :: success
    type(polynom_t) :: p1, p2, p_sum
    call p1%init ([1., 1.])
    call p2%init ([1., 1.])
    p_sum = p1 + p2
    success = all (assert ([2., 2.], p_sum%get_coeff()))
    success = success .and. assert (1, p_sum%deg ())
    success = success .and. assert (2., p_sum%lc ())
  end subroutine polynom_ut_operator_add_1

  subroutine polynom_ut_operator_add_2 (success)
    logical, intent(out) :: success
    type(polynom_t) :: p1, p2, p_sum
    call p1%init ([1., 1.])
    call p2%init ([1., -1.])
    p_sum = p1 + p2
    success = all (assert ([2.], p_sum%get_coeff()))
    success = success .and. assert (0, p_sum%deg ())
    success = success .and. assert (2., p_sum%lc ())
  end subroutine polynom_ut_operator_add_2

    subroutine polynom_ut_operator_add_3 (success)
    logical, intent(out) :: success
    type(polynom_t) :: p1, p2, p_sum
    call p1%init ([1., 1.])
    call p2%init ([1., -1., 3.])
    p_sum = p1 + p2
    success = all (assert ([2., 0., 3.], p_sum%get_coeff()))
    success = success .and. assert (2, p_sum%deg ())
    success = success .and. assert (3., p_sum%lc ())
  end subroutine polynom_ut_operator_add_3

  subroutine polynom_ut_operator_subtract_1 (success)
    logical, intent(out) :: success
    type(polynom_t) :: p1, p2, p_sum
    call p1%init ([2., 2.])
    call p2%init ([1., 1.])
    p_sum = p1 - p2
    success = all (assert ([1., 1.], p_sum%get_coeff()))
    success = success .and. assert (1, p_sum%deg ())
    success = success .and. assert (1., p_sum%lc ())
  end subroutine polynom_ut_operator_subtract_1

  subroutine polynom_ut_operator_subtract_2 (success)
    logical, intent(out) :: success
    type(polynom_t) :: p1, p2, p_sum
    call p1%init ([1., 1.])
    call p2%init ([-1., 1.])
    p_sum = p1 - p2
    success = all (assert ([2.], p_sum%get_coeff()))
    success = success .and. assert (0, p_sum%deg ())
    success = success .and. assert (2., p_sum%lc ())
  end subroutine polynom_ut_operator_subtract_2

  subroutine polynom_ut_operator_subtract_3 (success)
    logical, intent(out) :: success
    type(polynom_t) :: p1, p2, p_sum
    call p1%init ([1., 1.])
    call p2%init ([1., -1., 3.])
    p_sum = p1 - p2
    success = all (assert ([0., 2., -3.], p_sum%get_coeff()))
    success = success .and. assert (2, p_sum%deg ())
    success = success .and. assert (-3., p_sum%lc ())
  end subroutine polynom_ut_operator_subtract_3

  subroutine polynom_ut_operator_multiply_1 (success)
    logical, intent(out) :: success
    type(polynom_t) :: p1, p2, p
    call p1%init ([1.])
    call p2%init ([1., -1.])
    p = p1 * p2
    success = all (assert ([1., -1.], p%get_coeff ()))
  end subroutine polynom_ut_operator_multiply_1

  subroutine polynom_ut_operator_multiply_2 (success)
    logical, intent(out) :: success
    type(polynom_t) :: p1, p2, p
    call p1%init ([1., 1.])
    call p2%init ([1., -1.])
    p = p1 * p2
    success = all (assert ([1., 0., -1.], p%get_coeff ()))
    success = success .and. assert (2, p1%deg () + p2%deg ())
  end subroutine polynom_ut_operator_multiply_2

  subroutine polynom_ut_reduce_1 (success)
    logical, intent(out) :: success
    type(polynom_t) :: p
    call p%init ([1., 0., 0., 0., 0., 0. + tiny(1.)])
    call p%reduce ()
    success = all (assert ([1.], p%get_coeff ()))
    success = success .and. assert (0, p%deg ())
  end subroutine polynom_ut_reduce_1

  subroutine polynom_ut_reduce_2 (success)
    logical, intent(out) :: success
    type(polynom_t) :: p
    call p%init ([1., 1., 1., -1.])
    call p%reduce ()
    success = all (assert ([1., 1., 1., -1.], p%get_coeff ()))
    success = success .and. assert (3, p%deg ())
  end subroutine polynom_ut_reduce_2
end module polynom_ut
