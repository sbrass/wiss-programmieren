module integration
  use iso_fortran_env, only: i64 => INT64, r64 => REAL64
  use func

  implicit none

  private

  public :: cauchy_pv, trapez_rule, midpoint_rule, simpson_rule
contains
  !> Compute Cauchy principal value with f(x) / (x - c).
  function cauchy_pv (f, a, b, c, n) result (int)
    class(basic_func_t), intent(in) :: f
    real(r64), intent(in) :: a
    real(r64), intent(in) :: b
    real(r64), intent(in) :: c
    integer(i64), intent(in) :: n
    real(r64) :: int
    real(r64) :: h, xk
    integer(i64) :: i
    h = (b - a) / n
    xk = a - 0.5_r64 * h
    int = 0
    do i = 1, n
       xk = xk + h
       int = int + (f%evaluate (xk) - f%evaluate(c)) / (xk - c)
    end do
    int = h * int
  end function cauchy_pv

  function trapez_rule (f, a, b, n) result (int)
    class(basic_func_t), intent(in) :: f
    real(r64), intent(in) :: a
    real(r64), intent(in) :: b
    integer(i64), intent(in) :: n
    real(r64) :: int
    real(r64) :: h, xk
    integer(i64) :: i
    h = (b - a) / n
    xk = a
    int = 0
    do i = 1, n - 1
       xk = xk + h
       int = int + f%evaluate (xk)
    end do
    int = h * (int + 0.5_r64 * (f%evaluate (a) + f%evaluate (b)))
  end function trapez_rule

  function midpoint_rule (f, a, b, n) result (int)
    class(basic_func_t), intent(in) :: f
    real(r64), intent(in) :: a
    real(r64), intent(in) :: b
    integer(i64), intent(in) :: n
    real(r64) :: int
    real(r64) :: h, xk
    integer(i64) :: i
    h = (b - a) / n
    xk = a - 0.5_r64 * h
    int = 0
    do i = 1, n
       xk = xk + h
       int = int + f%evaluate (xk)
    end do
    int = h * int
  end function midpoint_rule

  function simpson_rule (f, a, b, n) result (int)
    class(basic_func_t), intent(in) :: f
    real(r64), intent(in) :: a
    real(r64), intent(in) :: b
    integer(i64), intent(in) :: n
    real(r64) :: int
    real(r64) :: h, xk
    integer(i64) :: i
    h = (b - a) / n
    xk = a
    int = f%evaluate (a) + f%evaluate (b)
    do i = 1, n / 2_i64 - 1
       !! 2k - 1
       xk = xk + h
       int = int + 4._r64 * f%evaluate (xk)
       !! 2k
       xk = xk + h
       int = int + 2._r64 * f%evaluate (xk)
    end do
    int = int + 4._r64 * f%evaluate (b - h)
    int = h * int / 3._r64
  end function simpson_rule
end module integration
