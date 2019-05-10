module derivatives
  use iso_fortran_env, only: r64 => REAL64
  use func

  implicit none

  private

  public :: naive_derive, symm_two_derive, symm_four_derive, symm_two_derive_snd
contains
  function naive_derive (func, x, h) result (y)
    class(basic_func_t), intent(in) :: func
    real(r64), intent(in) :: x, h
    real(r64) :: y
    y = func%evaluate(x + h) - func%evaluate(x)
    y = y / h
  end function naive_derive

  function symm_two_derive (func, x, h) result (y)
    class(basic_func_t), intent(in) :: func
    real(r64), intent(in) :: x, h
    real(r64) :: y
    y = func%evaluate(x + h) - func%evaluate(x - h)
    y = y / (2. * h)
  end function symm_two_derive

  function symm_four_derive (func, x, h) result (y)
    class(basic_func_t), intent(in) :: func
    real(r64), intent(in) :: x, h
    real(r64) :: y
    y = func%evaluate(x - 2 * h) - 8 * func%evaluate(x - h) + 8 * func%evaluate(x + h) - func%evaluate(x + 2 * h)
    y = y / (12. * h)
  end function symm_four_derive

  function symm_two_derive_snd (func, x, h) result (y)
    class(basic_func_t), intent(in) :: func
    real(r64), intent(in) :: x, h
    real(r64) :: y
    y = func%evaluate(x + h) - 2 * func%evaluate(x) + func%evaluate(x - h)
    y = y / h**2
  end function symm_two_derive_snd
end module derivatives
