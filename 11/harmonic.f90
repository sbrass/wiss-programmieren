module harmonic
  use iso_fortran_env, only: r64 => REAL64
  use solver, only: func_t

  implicit none

  private

  type, extends(func_t) :: harmonic_oscillator_t
     real(r64) :: mass
   contains
     procedure :: evaluate => harmonic_oscillator_evaluate
  end type harmonic_oscillator_t

  public :: harmonic_oscillator_t
contains
  elemental function harmonic_oscillator_evaluate (func, t, x) result (f)
    class(harmonic_oscillator_t), intent(in) :: func
    real(r64), intent(in) :: t
    real(r64), intent(in) :: x
    real(r64) :: f
    f = t
    f = - x / func%mass
  end function harmonic_oscillator_evaluate
end module harmonic
