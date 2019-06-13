module integration
  use iso_fortran_env, only: i64 => INT64, r64 => REAL64
  use func

  implicit none

  private

  interface multi_simpson_rule
     module procedure :: multi_simpson_rule_cmplx, multi_simpson_rule_real
  end interface multi_simpson_rule

  public :: cauchy_pv, trapez_rule, midpoint_rule, simpson_rule, multi_simpson_rule
contains
  !> Compute Cauchy principal value with f(x) / (x - c).
  function cauchy_pv (f, a, b, c, n) result (int)
    class(func_real_t), intent(in) :: f
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
    class(func_real_t), intent(in) :: f
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
    class(func_real_t), intent(in) :: f
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
    class(func_real_t), intent(in) :: f
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

  function multi_simpson_rule_cmplx (f, a, b, n) result (int)
    class(func_complex_t), intent(in) :: f
    real(r64), dimension(:), intent(in) :: a
    real(r64), dimension(:), intent(in) :: b
    integer(i64), intent(in) :: n
    complex(r64) :: int
    real(r64), dimension(:), allocatable :: h, xk
    integer(i64), dimension(:), allocatable :: box
    integer(i64) :: n_dim, i
    complex(r64) :: int_1d
    if (size (a) /= size (b)) then
       print *, "Unpassende Dimension der Grenzen."
    end if
    n_dim = size (a)
    allocate (h (n_dim))
    allocate (xk (n_dim))
    allocate (box (n_dim))
    !! Different h for each dimension, due to different interval size.
    h = (b - a) / n
    xk = a
    box = 1
    int = 0._r64
    outer: do
       !! begin - 1d-Simpson's rule
       xk(1) = a(1)
       int_1d = f%evaluate (xk)
       do i = 1, n / 2_i64 - 1
          !! 2k - 1
          xk(1) = xk(1) + h(1)
          int_1d = int_1d + 4._r64 * f%evaluate (xk)
          !! 2k
          xk(1) = xk(1) + h(1)
          int_1d = int_1d + 2._r64 * f%evaluate (xk)
       end do
       xk(1) = b(1) - h(1)
       int_1d = int_1d + 4._r64 * f%evaluate (xk)
       xk(1) = b(1)
       int_1d = int_1d + f%evaluate (xk)
       !! end - 1d-Simpson's rule
       !! Multiply with a combinatorial factor.
       int = int + compute_factor (box, n) * int_1d
       !! Advance indices in box and add i-dependent h(i) to xk(i).
       do i = 2, n_dim
          box(i) = box(i) + 1
          xk(i) = xk(i) + h(i)
          !! We have n+1 points to evaluate.
          if (mod (box(i) - 1, n + 1) /= 0) cycle outer
          box(i) = 1
          xk(i) = a(i)
       end do
       exit outer
    end do outer
    int = int * product (h) / 3.**n_dim
  contains
    function compute_factor (box, n) result (c)
      integer(i64), dimension(:), intent(in) :: box
      integer(i64), intent(in) :: n
      real(r64) :: c
      c = 1._r64
      do i = 2, size (box)
         if (box(i) == 1 .or. box(i) == (n + 1)) cycle
         if (mod (box(i), 2_i64) /= 0) then
            c = c * 4._r64 !! 2k - 1
         else
            c = c * 2._r64 !! 2k
         end if
      end do
    end function compute_factor
  end function multi_simpson_rule_cmplx

  function multi_simpson_rule_real (f, a, b, n) result (int)
    class(func_real_t), intent(in) :: f
    real(r64), dimension(:), intent(in) :: a
    real(r64), dimension(:), intent(in) :: b
    integer(i64), intent(in) :: n
    real(r64) :: int
    real(r64), dimension(:), allocatable :: h, xk
    integer(i64), dimension(:), allocatable :: box
    integer(i64) :: n_dim, i
    real(r64) :: int_1d
    if (size (a) /= size (b)) then
       print *, "Unpassende Dimension der Grenzen."
    end if
    n_dim = size (a)
    allocate (h (n_dim))
    allocate (xk (n_dim))
    allocate (box (n_dim))
    !! Different h for each dimension, due to different interval size.
    h = (b - a) / n
    xk = a
    box = 1
    int = 0._r64
    outer: do
       !! begin - 1d-Simpson's rule
       xk(1) = a(1)
       int_1d = f%evaluate (xk)
       do i = 1, n / 2_i64 - 1
          !! 2k - 1
          xk(1) = xk(1) + h(1)
          int_1d = int_1d + 4._r64 * f%evaluate (xk)
          !! 2k
          xk(1) = xk(1) + h(1)
          int_1d = int_1d + 2._r64 * f%evaluate (xk)
       end do
       xk(1) = b(1) - h(1)
       int_1d = int_1d + 4._r64 * f%evaluate (xk)
       xk(1) = b(1)
       int_1d = int_1d + f%evaluate (xk)
       !! end - 1d-Simpson's rule
       !! Multiply with a combinatorial factor.
       int = int + compute_factor (box, n) * int_1d
       !! Advance indices in box and add i-dependent h(i) to xk(i).
       do i = 2, n_dim
          box(i) = box(i) + 1
          xk(i) = xk(i) + h(i)
          !! We have n+1 points to evaluate.
          if (mod (box(i) - 1, n + 1) /= 0) cycle outer
          box(i) = 1
          xk(i) = a(i)
       end do
       exit outer
    end do outer
    int = int * product (h) / 3.**n_dim
  contains
    function compute_factor (box, n) result (c)
      integer(i64), dimension(:), intent(in) :: box
      integer(i64), intent(in) :: n
      real(r64) :: c
      c = 1._r64
      do i = 2, size (box)
         if (box(i) == 1 .or. box(i) == (n + 1)) cycle
         if (mod (box(i), 2_i64) /= 0) then
            c = c * 4._r64 !! 2k - 1
         else
            c = c * 2._r64 !! 2k
         end if
      end do
    end function compute_factor
  end function multi_simpson_rule_real

end module integration
