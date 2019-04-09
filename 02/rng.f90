module rng
  implicit none

  private

  public :: lcg, lcg01, xorshift

contains
  integer function lcg (state, a, c, m) result (r)
    integer, intent(in) :: state
    integer, intent(in) :: a
    integer, intent(in) :: c
    integer, intent(in) :: m
    r = mod (a * state + c, m)
  end function lcg

  real function lcg01 (state, a, c, m) result (r)
    real, intent(in) :: state
    integer, intent(in) :: a
    integer, intent(in) :: c
    integer, intent(in) :: m
    r = lcg(int(state * m), a, c, m) / m
  end function lcg01

  integer function xorshift (state, a1, a2, a3) result (r)
    integer, intent(in) :: state
    integer, intent(in) :: a1
    integer, intent(in) :: a2
    integer, intent(in) :: a3
    r = ieor (state, ishft (state, a1))
    r = ieor (r, ishft (r, -a2))
    r = ieor (r, ishft (r, a3))
    r = modulo (r, 2**30)
  end function xorshift
end module rng
