module unittest
  use iso_fortran_env, only: r64 => REAL64, r32 => REAL32, &
       i32 => INT32, i64 => INT64

  implicit none

  private

  interface assert
     module procedure :: assert_equal_real_single, assert_equal_double, &
          assert_equal_integer_single, assert_equal_integer_double
  end interface assert

  public :: assert
contains
  elemental function assert_equal_real_single (a, b) result (flag)
    real(r32), intent(in) :: a, b
    logical :: flag
    !! \todo check on NaN.
    flag = (abs(a - b) <= 2._r32 * epsilon(a))
  end function assert_equal_real_single

  elemental function assert_equal_double (a, b) result (flag)
    real(r64), intent(in) :: a, b
    logical :: flag
    !! \todo check on NanN.
    flag = (abs(a - b) <= 2._r64 * epsilon(a))
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
end module unittest
