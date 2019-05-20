module func
  use iso_fortran_env, only: r64 => REAL64
  implicit none

  private

  type, abstract :: basic_func_t
     private
   contains
     procedure(func_evaluate), deferred :: evaluate
  end type basic_func_t

  abstract interface
     function func_evaluate (func, x) result (y)
       import :: basic_func_t, r64
       class(basic_func_t), intent(in) :: func
       real(r64), intent(in) :: x
       real(r64) :: y
     end function func_evaluate
  end interface

  public :: basic_func_t
end module func
