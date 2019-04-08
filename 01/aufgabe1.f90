program main
  implicit none

  real, parameter :: a = 0.1234
  real :: x, f

  print *, "Aufgabe 1:"
  print *, "ε: ", epsilon(1.)

  print *, "Naive:"

  ! (i)
  x = 1.234e8 ! x >> 1
  f = 1. / sqrt(x) - 1. / sqrt (x + 1.)
  print *, "(i)   ", f

  ! (ii)
  x = 1.234e-8 ! x << 1
  f = (1. - cos (x)) / sin (x)
  print *, "(ii)  ", f

  ! (iii)
  x = 1.234e8 ! x >> a
  f = log (x + a) - log (x)
  print *, "(iii) ", f

  print *, "Improved:"

  ! (i)
  x = 1.234e8
  f = 0.5 * (x)**(-3. / 2.)
  print *, "(i)   ", f

  ! (ii)
  x = 1.234e8
  f = 0.5 * x
  print *, "(ii)  ", f

  ! (iii)
  x = 1.234e8
  f = x / a
  print *, "(iii) ", f
end program main
