module beugung
  use iso_fortran_env, only: r64 => REAL64
  use func

  implicit none

  private

  type, extends(basic_func_t) :: beugung_t
     ! real(r64), dimension(2) :: q
     real(r64) :: q
   contains
     procedure :: set_q => beugung_set_q
     procedure :: evaluate_1d_cmplx => beugung_evaluate_1d_cmplx
     procedure :: evaluate_nd_cmplx => beugung_evaluate_nd_cmplx
  end type beugung_t

  public :: beugung_t
contains
  subroutine beugung_set_q (func, q)
    class(beugung_t), intent(inout) :: func
    real(r64), intent(in) :: q
    func%q = q
  end subroutine beugung_set_q

  function beugung_evaluate_1d_cmplx (func, x) result (y)
    class(beugung_t), intent(in) :: func
    real(r64), intent(in) :: x
    complex(r64) :: y
  end function beugung_evaluate_1d_cmplx

  function beugung_evaluate_nd_cmplx (func, x) result (y)
    class(beugung_t), intent(in) :: func
    real(r64), dimension(:), intent(in) :: x
    complex(r64) :: y
    complex(r64), parameter :: i_ = cmplx (0._r64, 1._r64, r64)
    real(r64) :: r, phi, q
    r = x(1)
    phi = x(2)
    ! q = sqrt (dot_product (func%q, func%q))
    q = func%q
    y = r * exp (-i_ * q * r * cos (phi))
  end function beugung_evaluate_nd_cmplx
end module beugung
