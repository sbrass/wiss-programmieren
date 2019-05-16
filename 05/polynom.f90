module polynom
  use iso_fortran_env, only: r64 => REAL64
  use func

  implicit none

  private

  type, extends(basic_func_t) :: polynom_func_t
     private
     real(r64) :: a
     real(r64) :: n
   contains
     procedure :: init => polynom_func_init
     procedure :: write => polynom_func_write
     procedure :: evaluate => polynom_func_evaluate
  end type polynom_func_t

  public :: polynom_func_t
contains
  subroutine polynom_func_init (polynom, a, n)
    class(polynom_func_t), intent(out) :: polynom
    real(r64), intent(in) :: a, n
    polynom%a = a
    polynom%n = n
  end subroutine polynom_func_init

  subroutine polynom_func_write (polynom)
    class(polynom_func_t), intent(in) :: polynom
    write (*, "(A)") "a * x^n"
    write (*, *) "a = ", polynom%a
    write (*, *) "n = ", polynom%n
  end subroutine polynom_func_write

  function polynom_func_evaluate (func, x) result (y)
    class(polynom_func_t), intent(in) :: func
    real(r64), intent(in) :: x
    real(r64) :: y
    y = func%a * x**func%n
  end function polynom_func_evaluate
end module polynom
