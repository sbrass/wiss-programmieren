module hermite
  use iso_fortran_env, only: i64 => INT64, r64 => REAL64
  use func

  implicit none

  private

  type, extends(basic_func_t) :: hermite_t
     private
     integer(i64) :: n
     real(r64), dimension(:), allocatable :: h
   contains
     procedure :: init => hermite_init
     procedure :: write => hermite_write
     procedure :: evaluate => hermite_evaluate
  end type hermite_t

  public :: hermite_t
contains
  subroutine hermite_init (hermite, n)
    class(hermite_t), intent(out) :: hermite
    integer(i64), intent(in) :: n
    allocate (hermite%h(n + 1))
    hermite%n = n
    hermite%h = hermite_poly (n)
  end subroutine hermite_init

  recursive function hermite_poly (n) result (h)
    integer(i64), intent(in) :: n
    real(r64), dimension(n + 1) :: h
    real(r64), dimension(n + 1) :: hp, hm
    if (n == 0) then
       h(1) = 1
       return
    else if (n == 1) then
       h(1) = 2
       h(2) = 0
       return
    end if
    hp = 0
    hp(1:n) = 2 * hermite_poly (n - 1)
    hm = 0
    hm(3:) = 2 * (n - 1) * hermite_poly (n - 2)
    h = hp - hm
  end function hermite_poly

  subroutine hermite_write (hermite)
    class(hermite_t), intent(in) :: hermite
    integer(i64) :: i
    print *, hermite%n
    print *, hermite%h(hermite%n + 1)
    do i = hermite%n, 1, -1
       print *, hermite%h(i), "x**", hermite%n - i + 1
    end do
  end subroutine hermite_write

  function hermite_evaluate (func, x) result (y)
    class(hermite_t), intent(in) :: func
    real(r64), intent(in) :: x
    real(r64) :: y
    integer(i64) :: i
    y = func%h(func%n + 1)
    do i = func%n, 1, -1
       y = y + func%h(i) * x**(func%n - i + 1)
    end do
  end function hermite_evaluate
end module hermite
