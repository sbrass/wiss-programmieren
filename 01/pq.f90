program main
  implicit none

  real :: p, q
  real :: x1, x2

  p = 2500
  q = 0.001

  print *, "ε: ", epsilon(1.)
  print *, "ΔE: ", log10( abs(p)) - log10 (abs (q))

  ! Naive.
  x1 = -p / 2 + sqrt (p**2 / 4 - q)
  x2 = -p / 2 - sqrt (p**2 / 4 - q)

  print *, "Naive:"
  print *, x1, x2

  ! Improved.
  x2 = -p / 2 - sqrt (p**2 / 4 - q)
  x1 = q / x2

  print *, "Improved:"
  print *, x1, x2
end program main
