program main
  use iso_fortran_env, only: r64 => REAL64

  real(r64), parameter :: eps = 1e-7

  real(r64), dimension(3) :: x0 = [-2, 0, 2]
  real(r64), dimension(3) :: y0 = [0, 2, 4]
  real(r64), dimension(3) :: z

  do i = 1, 3
     z(i) = bisection(f1, x0(i), y0(i), eps)
  end do
  print *, "Nullstellen für: 0.5 * x**3 - 2 * x + x + 1: ", z

  x0 = [0, 1, 0]
  y0 = [1, 2, 0]
  do i = 1, 2
     z(i) = bisection(f2, x0(i), y0(i), eps)
  end do
  print *, "Nullstellen für: 3 * x + sin (x) - exp (x): ", z(1:2)
contains
  function f1 (x)
    real(r64), intent(in) :: x
    real(r64) :: f1
    f1 = 0.5 * x**3 - 2 * x**2 + x + 1
  end function f1

  function f2 (x)
    real(r64), intent(in) :: x
    real(r64) :: f2
    f2 = 3 * x + sin (x) - exp (x)
  end function f2

  function bisection (f, x0, y0, eps) result (z)
    !! Function as an argument.
    interface
       function f(x)
         import :: r64
         real(r64), intent(in) :: x
         real(r64) :: f
       end function f
    end interface
    real(r64), intent(in) :: x0, y0, eps
    real(r64) :: z
    real(r64) :: x, y, fx, fz
    !! \todo{sbrass} Check existence of root, f(a) * f(b) < 0.
    x = x0; y = y0
    z = (x0 + y0) / 2
    do while (y - x > eps)
       fx = f(x)
       fz = f(z)
       if (fz * fx < 0) then
          y = z
       else
          x = z
       end if
       z = (x + y) / 2
    end do
  end function bisection
end program main
