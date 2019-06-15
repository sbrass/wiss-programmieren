program main
  use iso_fortran_env, only: r64 => REAL64, i64 => INT64

  use polynom_ut

  implicit none

  integer :: n_tests = 0, n_tests_passed = 0

  call run_ut (polynom_ut_init_coefficients, &
       "polynom%init", "Initialisieren eines Polynom mit Koeffizienten-Array.")
  call run_ut (polynom_ut_init_single_coefficient, &
       "polynom%init", "Initialisieren eines Polynom mit führendem Koeffizient und Grad n.")
  call run_ut (polynom_ut_operator_add_1, &
       "p1(n) + p2(n) = p(n)", "Addieren von zwei Polynomen gleichen Grades.")
  call run_ut (polynom_ut_operator_add_2, &
       "p1(n) + p2(n) = p(m)", "Addieren von zwei Polynomen gleichen Grades, ungleich dem Grad des Ergebnis.")
  call run_ut (polynom_ut_operator_add_3, &
       "p1(n) + p2(m) = p(m)", "Addieren von zwei Polynomen ungleichen Grades.")
  call run_ut (polynom_ut_operator_subtract_1, &
       "p1(n) + p2(n) = p(n)", "Subtrahieren von zwei Polynomen gleichen Grades.")
  call run_ut (polynom_ut_operator_subtract_2, &
       "p1(n) + p2(n) = p(m)", "Subtrahieren von zwei Polynomen gleichen Grades, ungleich dem Grad des Ergebnis.")
  call run_ut (polynom_ut_operator_subtract_3, &
       "p1(n) + p2(m) = p(m)", "Subtrahierem von zwei Polynomen ungleichen Grades.")
  call run_ut (polynom_ut_operator_multiply_1, &
       "p1(0) * p2(n) = a0 * p2(n)", "Multiplizieren von Polynom Grad 0 und Polynom Grad n.")
  call run_ut (polynom_ut_operator_multiply_2, &
       "p1(n) * p2(m) = p(n + m)", "Multiplizieren von Polynomen gleichen Grades.")
  call run_ut (polynom_ut_reduce_1, &
       "p(m) -> p(n), m >= n", "Reduce to leading coefficient.")
  call run_ut (polynom_ut_reduce_2, &
       "p(m) -> p(n), m >= n", "Nothing to reduce.")

  write (*, "(A,I3,A,I3,A)") "Zusammenfassung: [", n_tests_passed, "/", n_tests, "]"
contains
  subroutine run_ut (func_ut, name, description)
    interface
       subroutine func_ut (success)
         logical, intent(out) :: success
       end subroutine func_ut
    end interface
    character(len=*), intent(in) :: name, description
    logical :: success
    call func_ut (success)
    if (.not. success)  then
       write (*, "(A,1X,A)") "Test:", name
       write (*, "(2X,A)") description
       write (*, "(2X,A,1X,L1)") "Bestanden?", success
    end if
    n_tests = n_tests + 1
    if (success) n_tests_passed = n_tests_passed + 1
  end subroutine run_ut
end program main
