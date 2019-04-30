module rng
  use iso_fortran_env, only: i64 => INT64
  implicit none

  private

  public :: lcg, xorshift, xorshift64

contains
  integer(i64) function lcg (state, a, c, m) result (r)
    integer(i64), intent(in) :: state
    integer(i64), intent(in) :: a
    integer(i64), intent(in) :: c
    integer(i64), intent(in) :: m
    r = mod (a * state + c, m)
  end function lcg

  integer function xorshift (state, a1, a2, a3) result (r)
    integer, intent(in) :: state
    integer, intent(in) :: a1
    integer, intent(in) :: a2
    integer, intent(in) :: a3
    r = ieor (state, ishft (state, a1))
    r = ieor (r, ishft (r, -a2))
    r = ieor (r, ishft (r, a3))
    !! Modulo: respect sign of divisor.
    r = modulo (r, 2**30)
  end function xorshift

  integer(i64) function xorshift64 (state, a1, a2, a3) result (r)
    integer(i64), intent(in) :: state
    integer(i64), intent(in) :: a1
    integer(i64), intent(in) :: a2
    integer(i64), intent(in) :: a3
    r = ieor (state, ishft (state, a1))
    r = ieor (r, ishft (r, -a2))
    r = ieor (r, ishft (r, a3))
    r = mod (r, 2_i64**62)
  end function xorshift64
end module rng
