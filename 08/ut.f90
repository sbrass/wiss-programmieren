program main
  use, intrinsic :: ieee_features
  use, intrinsic :: ieee_arithmetic
  use unit_test, only: assert

  implicit none

  print *, assert (1., ieee_value(1., ieee_positive_inf))
  print *, assert (1., ieee_value(1., ieee_quiet_nan))
  print *, assert (ieee_value(1., ieee_quiet_nan), 1.)
  print *, assert (ieee_value(1., ieee_quiet_nan), ieee_value(1., ieee_positive_inf))
  print *, assert (1., 1.)
end program main
