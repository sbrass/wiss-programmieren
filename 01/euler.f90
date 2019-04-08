program main

  implicit none

  integer, parameter :: n = 100
  real, parameter :: h = 0.5
  real, parameter :: y0 = 1, y1 = 0.5
  real :: y, y_last, y_sym, y0_sym, y1_sym
  integer :: i

  y_last = y0
  y0_sym = y0
  y1_sym = y1
  do i = 1, n
     y = euler(y_last, h)
     y_sym = euler_symmetric (y0_sym, y1_sym, h)
     y0_sym = y1_sym
     y1_sym = y_sym
     print *, i * h, y, y_sym
     y_last = y
  end do

contains
  real function euler (y0, h) result (y)
    real, intent(in) :: y0
    real, intent(in) :: h
    y = y0 * (1 - h)
  end function euler

  real function error_euler (y0, y, i, h) result (err)
    real, intent(in) :: y0
    real, intent(in) :: y
    integer, intent(in) :: i
    real, intent(in) :: h
    err = i * h * y0 / y
  end function error_euler

  real function euler_symmetric (y0, y1, h) result (y)
    real, intent(in) :: y0
    real, intent(in) :: y1
    real, intent(in) :: h
    y = y0 - 2. * y1 * h
  end function euler_symmetric

  real function real_error (f, f_d, x) result (err)
    real, intent(in) :: f
    real, intent(in) :: f_d
    real, intent(in) :: x
    err = x * f_d / f
  end function real_error
end program main
