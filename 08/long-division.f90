module long_division
  use iso_fortran_env, only: OUTPUT_UNIT
  use polynom
  use divisor

  implicit none

  private

  type, extends(polynom_divisor_t) :: long_division_t
     private
   contains
     procedure :: write => long_division_write
     procedure :: divide => long_division_divide
     procedure :: quotient => long_division_get_quotient
     procedure :: remainder => long_division_get_remainder
  end type long_division_t

  public :: long_division_t
contains
  subroutine long_division_write (d, u)
    class(long_division_t), intent(in) :: d
    integer, intent(in) :: u
    call d%base_write ("Long Division Algorithm", u)
  end subroutine long_division_write

  subroutine long_division_divide (d, a, b)
    class(long_division_t), intent(out) :: d
    type(polynom_t), intent(in) :: a, b
    integer :: deg, c
    type(polynom_t) :: s
    if (b%deg () == 0) then
       d%q = a
    else
       associate (q => d%q, r => d%r)
         call q%init (0, 0.)
         ! q = polynom_single_coeff (0, 0.) !! zero
         r = a
         deg = b%deg ()
         c = b%lc ()
         do while (r%deg () >= deg)
            call s%init (r%deg() - deg, r%lc () / c)
            q = q + s
            r = r - s * b
         end do
       end associate
    end if
  end subroutine long_division_divide

  type(polynom_t) function long_division_get_quotient (d) result (p)
    class(long_division_t), intent(in) :: d
    p = d%q
  end function long_division_get_quotient

  type(polynom_t) function long_division_get_remainder (d) result (p)
    class(long_division_t), intent(in) :: d
    p = d%r
  end function long_division_get_remainder
end module long_division
