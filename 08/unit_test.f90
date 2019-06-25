module unit_test
  use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
  use, intrinsic :: iso_fortran_env, only: r64 => REAL64, r32 => REAL32, &
       i32 => INT32, i64 => INT64

  implicit none

  private

  type :: unit_test_t
     integer :: n_tests = 0
     integer :: n_tests_failed = 0
     integer :: verbose = 1
   contains
     procedure :: run_ut => unit_test_run_ut
     procedure :: write => unit_test_write
  end type unit_test_t

  interface assert
     module procedure :: assert_equal_real_single, assert_equal_double, &
          assert_equal_integer_single, assert_equal_integer_double
  end interface assert

  public :: assert, unit_test_t
contains
  subroutine unit_test_run_ut (ut, func_ut, name, description)
    class(unit_test_t), intent(inout) :: ut
    interface
       subroutine func_ut (success)
         logical, intent(out) :: success
       end subroutine func_ut
    end interface
    character(len=*), intent(in) :: name, description
    logical :: success
    call func_ut (success)
    if (ut%verbose > 0) then
       write (*, "(A,1X,A,1X,A,1X,L1)") "Test:", name, ":", success
    end if
    if (ut%verbose > 1) then
       write (*, "(2X,A)") description
    end if
    ut%n_tests = ut%n_tests + 1
    if (.not. success) ut%n_tests_failed = ut%n_tests_failed + 1
  end subroutine unit_test_run_ut

  subroutine unit_test_write (ut)
    class(unit_test_t), intent(in) :: ut
    write (*, "(A,I3,A,I3,A)") "Zusammenfassung: [", ut%n_tests - ut%n_tests_failed, "/", ut%n_tests, "]"
  end subroutine unit_test_write

  elemental function assert_equal_real_single (a, b) result (flag)
    real(r32), intent(in) :: a, b
    logical :: flag
    if (.not. ieee_is_finite (a) .or. .not. ieee_is_finite (b)) then
       flag = .false.
    end if
    flag = (abs(a - b) <= 10._r32 * epsilon(a))
  end function assert_equal_real_single

  elemental function assert_equal_double (a, b) result (flag)
    real(r64), intent(in) :: a, b
    logical :: flag
    if (.not. ieee_is_finite (a) .or. .not. ieee_is_finite (b)) then
       flag = .false.
    end if
    flag = (abs(a - b) <= 10._r64 * epsilon(a))
  end function assert_equal_double

  elemental function assert_equal_integer_single (a, b) result (flag)
    integer(i32), intent(in) :: a, b
    logical :: flag
    flag = (a == b)
  end function assert_equal_integer_single

  elemental function assert_equal_integer_double (a, b) result (flag)
    integer(i64), intent(in) :: a, b
    logical :: flag
    flag = (a == b)
  end function assert_equal_integer_double
end module unit_test
