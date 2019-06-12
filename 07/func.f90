module func
  use iso_fortran_env, only: r64 => REAL64
  implicit none

  private

  type, abstract :: basic_func_t
     private
   contains
     ! procedure(func_evaluate_1d), deferred :: evaluate_1d
     ! procedure(func_evaluate_nd), deferred :: evaluate_nd
     procedure(func_evaluate_1d_cmplx), deferred :: evaluate_1d_cmplx
     procedure(func_evaluate_nd_cmplx), deferred :: evaluate_nd_cmplx
     ! generic :: evaluate => evaluate_1d, evaluate_nd, evaluate_1d_cmplx, evaluate_nd_cmplx
     generic :: evaluate => evaluate_1d_cmplx, evaluate_nd_cmplx
  end type basic_func_t

  abstract interface
     ! function func_evaluate_1d (func, x) result (y)
     !   import :: basic_func_t, r64
     !   class(basic_func_t), intent(in) :: func
     !   real(r64), intent(in) :: x
     !   real(r64) :: y
     ! end function func_evaluate_1d

     ! function func_evaluate_nd (func, x) result (y)
     !   import :: basic_func_t, r64
     !   class(basic_func_t), intent(in) :: func
     !   real(r64), dimension(:), intent(in) :: x
     !   real(r64) :: y
     ! end function func_evaluate_nd

     function func_evaluate_1d_cmplx (func, x) result (y)
       import :: basic_func_t, r64
       class(basic_func_t), intent(in) :: func
       real(r64), intent(in) :: x
       complex(r64) :: y
     end function func_evaluate_1d_cmplx

     function func_evaluate_nd_cmplx (func, x) result (y)
       import :: basic_func_t, r64
       class(basic_func_t), intent(in) :: func
       real(r64), dimension(:), intent(in) :: x
       complex(r64) :: y
     end function func_evaluate_nd_cmplx
  end interface

  public :: basic_func_t
end module func
