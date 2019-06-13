module beugung
  use iso_fortran_env, only: r64 => REAL64
  use func

  implicit none

  private

  type, extends(func_complex_t) :: beugung_t
     real(r64) :: q, theta
   contains
     generic :: set_q => set_q_single, set_q_multi
     procedure :: set_q_single => beugung_set_q_single
     procedure :: set_q_multi => beugung_set_q_multi
     procedure :: evaluate_1d_cmplx => beugung_evaluate_1d_cmplx
     procedure :: evaluate_nd_cmplx => beugung_evaluate_nd_cmplx
  end type beugung_t

  public :: beugung_t
contains
  subroutine beugung_set_q_single (func, q)
    class(beugung_t), intent(inout) :: func
    real(r64), intent(in) :: q
    func%q = q
    func%theta = 0
  end subroutine beugung_set_q_single

  subroutine beugung_set_q_multi (func, q)
    class(beugung_t), intent(inout) :: func
    real(r64), dimension(:), intent(in) :: q
    func%q = sqrt (dot_product (q, q))
    !! Angle between (q, e1).
    func%theta = acos (q(1) / func%q)
  end subroutine beugung_set_q_multi

  function beugung_evaluate_1d_cmplx (func, x) result (y)
    class(beugung_t), intent(in) :: func
    real(r64), intent(in) :: x
    complex(r64) :: y
  end function beugung_evaluate_1d_cmplx

  function beugung_multi_evaluate_1d_cmplx (func, x) result (y)
    class(beugung_t), intent(in) :: func
    real(r64), intent(in) :: x
    complex(r64) :: y
  end function beugung_multi_evaluate_1d_cmplx

  function beugung_evaluate_nd_cmplx (func, x) result (y)
    class(beugung_t), intent(in) :: func
    real(r64), dimension(:), intent(in) :: x
    complex(r64) :: y
    complex(r64), parameter :: i_ = cmplx (0._r64, 1._r64, r64)
    real(r64) :: r, phi
    r = x(1)
    phi = x(2)
    y = r * exp (-i_ * func%q * r * cos (phi - func%theta))
  end function beugung_evaluate_nd_cmplx
end module beugung
