module func
  use iso_fortran_env, only: r64 => REAL64
  implicit none

  private

  type, abstract :: func_real_t
     private
   contains
     procedure(func_evaluate_1d_real), deferred :: evaluate_1d_real
     procedure(func_evaluate_nd_real), deferred :: evaluate_nd_real
     generic :: evaluate => evaluate_1d_real, evaluate_nd_real
  end type func_real_t

  type, abstract :: func_complex_t
     private
   contains
     procedure(func_evaluate_1d_cmplx), deferred :: evaluate_1d_cmplx
     procedure(func_evaluate_nd_cmplx), deferred :: evaluate_nd_cmplx
     generic :: evaluate => evaluate_1d_cmplx, evaluate_nd_cmplx
  end type func_complex_t

  abstract interface
     function func_evaluate_1d_real (func, x) result (y)
       import :: func_real_t, r64
       class(func_real_t), intent(in) :: func
       real(r64), intent(in) :: x
       real(r64) :: y
     end function func_evaluate_1d_real

     function func_evaluate_nd_real (func, x) result (y)
       import :: func_real_t, r64
       class(func_real_t), intent(in) :: func
       real(r64), dimension(:), intent(in) :: x
       real(r64) :: y
     end function func_evaluate_nd_real
  end interface

  abstract interface
     function func_evaluate_1d_cmplx (func, x) result (y)
       import :: func_complex_t, r64
       class(func_complex_t), intent(in) :: func
       real(r64), intent(in) :: x
       complex(r64) :: y
     end function func_evaluate_1d_cmplx

     function func_evaluate_nd_cmplx (func, x) result (y)
       import :: func_complex_t, r64
       class(func_complex_t), intent(in) :: func
       real(r64), dimension(:), intent(in) :: x
       complex(r64) :: y
     end function func_evaluate_nd_cmplx
  end interface

  public :: func_real_t, func_complex_t
end module func
