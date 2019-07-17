module solver
  use iso_fortran_env, only: ERROR_UNIT, OUTPUT_UNIT, r64 => REAL64, i64 => INT64

  implicit none

  private

  type, abstract :: func_t
   contains
     procedure(func_evaluate), deferred :: evaluate
  end type func_t

  abstract interface
     elemental function func_evaluate (func, t, x) result (f)
       import :: r64, func_t
       class(func_t), intent(in) :: func
       real(r64), intent(in) :: t
       real(r64), intent(in) :: x
       real(r64) :: f
     end function func_evaluate
  end interface

  public :: func_t

  type :: newton_solver_t
     real(r64) :: t0 = 0
     real(r64) :: tn = 0
     real(r64) :: h = 0
     integer(r64) :: n
     !! State
     real(r64) :: t = 0
   contains
     procedure :: init => newton_solver_init
     procedure :: get_t => newton_solver_get_t
     procedure :: solve_step => newton_solver_solve_step
     procedure :: solve => newton_solver_solve
  end type newton_solver_t

  public :: newton_solver_t
contains
  subroutine newton_solver_init (solver, t0, tn, n)
    class(newton_solver_t), intent(out) :: solver
    real(r64), intent(in) :: t0, tn
    integer(i64), intent(in) :: n
    solver%t0 = t0
    solver%tn = tn
    solver%n = n
    solver%h = (tn - t0) / n
    solver%t = t0
  end subroutine newton_solver_init
  real(r64) function newton_solver_get_t (solver) result (tn)
    class(newton_solver_t), intent(in) :: solver
    tn = solver%t
  end function newton_solver_get_t
  subroutine newton_solver_solve_step (solver, ext_field, r, v)
    class(newton_solver_t), intent(inout) :: solver
    class(func_t), dimension(:), intent(in) :: ext_field
    real(r64), dimension(size (ext_field)), intent(inout) :: r, v
    real(r64), dimension(size (ext_field)) :: v_old
    !! Check sizes of ext_field, r and v.
    !! Cache v, as we need the old one, to compute the next rₙ.
    v_old = v
    !! Euler's method.
    v = v + ext_field%evaluate (solver%t, r) * solver%h
    r = r + v_old * solver%h
    solver%t = solver%t + solver%h
  end subroutine newton_solver_solve_step
  subroutine newton_solver_solve (solver, ext_field, r, v)
    class(newton_solver_t), intent(inout) :: solver
    class(func_t), dimension(:), intent(in) :: ext_field
    real(r64), dimension(:), intent(inout) :: r, v
    !! Check sizes of ext_field, r and v.
    do while (solver%t <= solver%tn)
       call solver%solve_step (ext_field, r, v)
    end do
  end subroutine newton_solver_solve
end module solver
