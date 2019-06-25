program main
  use unit_test
  use long_division_ut

  implicit none

  type(unit_test_t) :: ut

  call ut%run_ut (long_division_ut_divide_1, &
       "a(n) / b(n) = 1", "Dividieren von zwei identischen Polynomen.")
  call ut%run_ut (long_division_ut_divide_2, &
       "a(n) / b(m), n > m", "Dividieren von zwei unterschiedlichen Polynomen mit n > m.")
  call ut%run_ut (long_division_ut_divide_3, &
       "a(n) / b(m) → a = b * q + r, n > m", "Dividieren von zwei unterschiedlichen Polynomen mit n > m und b * q + r. ")
  call ut%run_ut (long_division_ut_divide_4, &
       "a(n) / b(m), n < m", "Dividieren von zwei unterschiedlichen Polynomen mit n < m.")
  call ut%run_ut (long_division_ut_divide_5, &
       "a(n) / c", "Dividieren eines Polynoms mit Konstante.")
  call ut%run_ut (long_division_ut_divide_6, &
       "a(n) / 0", "Dividieren eines Polynoms mit Null.")

  call ut%write ()
end program main
