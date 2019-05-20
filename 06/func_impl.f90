module func_impl
  use iso_fortran_env, only: r64 => REAL64
  use func

  implicit none

  private

  type, extends(basic_func_t) :: func1_t
   contains
     procedure :: evaluate => func1_evaluate
  end type func1_t

  type, extends(basic_func_t) :: func2_t
   contains
     procedure :: evaluate => func2_evaluate
  end type func2_t

  type, extends(basic_func_t) :: func3_t
   contains
     procedure :: evaluate => func3_evaluate
  end type func3_t

  type, extends(basic_func_t) :: func4_t
   contains
     procedure :: evaluate => func4_evaluate
  end type func4_t

  public :: func1_t, func2_t, func3_t, func4_t
contains
  function func1_evaluate (func, x) result (y)
    class(func1_t), intent(in) :: func
    real(r64), intent(in) :: x
    real(r64) :: y
    y = exp (-x) / x
  end function func1_evaluate

  function func2_evaluate (func, x) result (y)
    class(func2_t), intent(in) :: func
    real(r64), intent(in) :: x
    real(r64) :: y
    y = x * sin (1 / x)
  end function func2_evaluate

  function func3_evaluate (func, x) result (y)
    class(func3_t), intent(in) :: func
    real(r64), intent(in) :: x
    real(r64) :: y
    y = exp(x)
  end function func3_evaluate

  function func4_evaluate (func, x) result (y)
    class(func4_t), intent(in) :: func
    real(r64), intent(in) :: x
    real(r64) :: y
    y = exp(-x**2)
  end function func4_evaluate
end module func_impl
