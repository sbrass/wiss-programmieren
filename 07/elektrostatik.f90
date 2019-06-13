module elektrostatik
  use iso_fortran_env, only: i64 => INT64, r64 => REAL64
  use func

  implicit none

  private

  type, extends(func_real_t) :: potential_static_t
     real(r64) :: x
   contains
     procedure :: set_x => potential_static_set_x
     procedure :: evaluate_1d_real => potential_static_evaluate_1d_real
     procedure :: evaluate_nd_real => potential_static_evaluate_nd_real
  end type potential_static_t

  type, extends(func_real_t) :: potential_dynamic_t
     real(r64) :: x
   contains
     procedure :: set_x => potential_dynamic_set_x
     procedure :: evaluate_1d_real => potential_dynamic_evaluate_1d_real
     procedure :: evaluate_nd_real => potential_dynamic_evaluate_nd_real
  end type potential_dynamic_t

  public :: potential_static_t, potential_dynamic_t
contains
  subroutine potential_static_set_x (func, x)
    class(potential_static_t), intent(inout) :: func
    real(r64), intent(in) :: x
    func%x = x
  end subroutine potential_static_set_x

  function potential_static_evaluate_1d_real (func, x) result (y)
    class(potential_static_t), intent(in) :: func
    real(r64), intent(in) :: x
    real(r64) :: y
  end function potential_static_evaluate_1d_real

  function potential_static_evaluate_nd_real (func, x) result (y)
    class(potential_static_t), intent(in) :: func
    real(r64), dimension(:), intent(in) :: x
    real(r64) :: y
    !! Numeric stabilization for very small values.
    if (abs(func%x - x(1)) < 1e-10 .and. all(abs(x(2:3)) < 1e-10)) then
       y = 0
    else
       y = 1. / sqrt ((func%x - x(1))**2 + x(2)**2 + x(3)**2)
    end if
  end function potential_static_evaluate_nd_real

  subroutine potential_dynamic_set_x (func, x)
    class(potential_dynamic_t), intent(inout) :: func
    real(r64), intent(in) :: x
    func%x = x
  end subroutine potential_dynamic_set_x

  function potential_dynamic_evaluate_1d_real (func, x) result (y)
    class(potential_dynamic_t), intent(in) :: func
    real(r64), intent(in) :: x
    real(r64) :: y
  end function potential_dynamic_evaluate_1d_real

  function potential_dynamic_evaluate_nd_real (func, x) result (y)
    class(potential_dynamic_t), intent(in) :: func
    real(r64), dimension(:), intent(in) :: x
    real(r64) :: y
    !! Numeric stabilization for very small values.
    if (abs(func%x - x(1)) < 1e-10 .and. all(abs(x(2:3)) < 1e-10)) then
       y = 0
    else
       y = x(1) / sqrt ((func%x - x(1))**2 + x(2)**2 + x(3)**2)
    end if
  end function potential_dynamic_evaluate_nd_real

end module elektrostatik
