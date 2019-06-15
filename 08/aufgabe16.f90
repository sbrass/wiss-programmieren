program main
  use iso_fortran_env, only: r64 => REAL64, i64 => INT64

  use unit_test
  use polynom_ut

  implicit none

  type(unit_test_t) :: ut

  call ut%run_ut (polynom_ut_init_coefficients, &
       "polynom%init (Array)", "Initialisieren eines Polynom mit Koeffizienten-Array.")
  call ut%run_ut (polynom_ut_init_single_coefficient, &
       "polynom%init (C, N)", "Initialisieren eines Polynom mit führendem Koeffizient und Grad n.")
  call ut%run_ut (polynom_ut_operator_add_1, &
       "p1(n) + p2(n) = p(n)", "Addieren von zwei Polynomen gleichen Grades.")
  call ut%run_ut (polynom_ut_operator_add_2, &
       "p1(n) + p2(n) = p(m)", "Addieren von zwei Polynomen gleichen Grades, ungleich dem Grad des Ergebnis.")
  call ut%run_ut (polynom_ut_operator_add_3, &
       "p1(n) + p2(m) = p(m)", "Addieren von zwei Polynomen ungleichen Grades.")
  call ut%run_ut (polynom_ut_operator_subtract_1, &
       "p1(n) + p2(n) = p(n)", "Subtrahieren von zwei Polynomen gleichen Grades.")
  call ut%run_ut (polynom_ut_operator_subtract_2, &
       "p1(n) + p2(n) = p(m)", "Subtrahieren von zwei Polynomen gleichen Grades, ungleich dem Grad des Ergebnis.")
  call ut%run_ut (polynom_ut_operator_subtract_3, &
       "p1(n) + p2(m) = p(m)", "Subtrahierem von zwei Polynomen ungleichen Grades.")
  call ut%run_ut (polynom_ut_operator_multiply_1, &
       "p1(0) * p2(n) = a0 * p2(n)", "Multiplizieren von Polynom Grad 0 und Polynom Grad n.")
  call ut%run_ut (polynom_ut_operator_multiply_2, &
       "p1(n) * p2(m) = p(n + m)", "Multiplizieren von Polynomen gleichen Grades.")
  call ut%run_ut (polynom_ut_reduce_1, &
       "p(m) -> p(n), m >= n", "Reduce to leading coefficient.")
  call ut%run_ut (polynom_ut_reduce_2, &
       "p(m) -> p(m), m == n", "Nothing to reduce.")

  call ut%write ()
end program main
